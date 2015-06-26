/*
 * Parser.cpp
 *
 *  Created on: Jun 18, 2015
 *      Author: torin
 */


#include "Parser.hpp"


Parser::Parser(llvm::Module* module, std::string filename, std::ifstream* stream) {
	this->module = module;
	lexer = new Lexer(filename, stream);
	codeGenerator = new CodeGenerator(module);

	CreateType("Void", llvm::Type::getVoidTy(module->getContext()));
	CreateType("S8", llvm::Type::getInt8Ty(module->getContext()));
	CreateType("S16", llvm::Type::getInt16Ty(module->getContext()));
	CreateType("S32", llvm::Type::getInt32Ty(module->getContext()));
	CreateType("S64", llvm::Type::getInt64Ty(module->getContext()));

	CreateType("F16", llvm::Type::getHalfTy(module->getContext()));
	CreateType("F32", llvm::Type::getFloatTy(module->getContext()));
	CreateType("F64", llvm::Type::getDoubleTy(module->getContext()));
	CreateType("F128", llvm::Type::getFP128Ty(module->getContext()));
}

Parser::~Parser() {
	delete lexer;
	delete codeGenerator;
}

void Parser::CreateType(std::string name, llvm::Type* type) {
	types.push_back(ASTType(name, type));
	typeMap[name] = types.size();
}

ASTExpression* Parser::ParseExpression() {
	LOG_VERBOSE("Parsing Expression :: NOT IMPLEMENTED RETURNS DUMMY VARIABLE");
	return new ASTVariable("dummy", StringToType("Int32"));
}


ASTType* Parser::StringToType(std::string string) {
	uint32 typeIndex = typeMap[string];
	if(typeIndex == 0) {
		LOG_DEBUG(string << " has not been defined!");
		return nullptr;
	}
	ASTType* type = &types[typeIndex - 1];
	return type;
}

ASTFunction* Parser::ParseFunction(std::string& identiferName) {
	LOG_VERBOSE("Parsing Function");

	//We should be at the openParen
	//Getting the nextToken will eat it and we will see what args are inside
	lexer->NextToken();
	//Loop through tokens until we find the end of the Paren
	std::vector<Type> args;
	while(lexer->token != Token::ParenClose && lexer->token != Token::EndOfFile && lexer->token != Token::Unkown) {
		//TODO care about function parameters
	}

	if(lexer->token == Token::ParenClose) {
		lexer->NextToken();
	} else {
		LOG_ERROR("Expected end of parens in function decl");
	}

	ASTFunction* function = new ASTFunction(identiferName, args);
	if (lexer->token == Token::TypeReturn) {
		lexer->NextToken();
		if(lexer->token != Token::Identifer) {
			LOG_ERROR("expected a type after the return operator");
			return nullptr;
		}

		function->returnType = StringToType(lexer->tokenString);
		lexer->NextToken();
	} else {
		function->returnType = &types[0];
	}

	if(lexer->token == Token::ScopeOpen) {
		//A new scope has been opened...
		while(lexer->token != Token::ScopeClose && lexer->token != Token::EndOfFile) {
			ASTNode* node = ParsePrimary();
			if(node == nullptr) {
				LOG_ERROR("Could not generate code for statement inside function body: " << identiferName);
				return nullptr;
			}
			function->body.push_back(node);
		}

	} else if (lexer->token != Token::Foreign) {
		LOG_ERROR("Expected a new scope to open '{' after function definition!");
		return nullptr;
	}

	codeGenerator->Codegen(function);
	return function;
}



ASTType* Parser::ParseType() {
	//Eat the previous token and get the type
	lexer->NextToken();
	ASTType* type = StringToType(lexer->tokenString);
	return type;
}

ASTNode* Parser::ParseIdentifier() {
	LOG_VERBOSE("Parsing Identifier : " << lexer->tokenString);
	std::string name = lexer->tokenString;
	ASTType* type;

	FilePosition identifierFilePos = lexer->filePos; //Store the position in the file the identifier was seen

	lexer->NextToken();	//Eat the identifier

	//TYPEASSIGNMENT
	if (lexer->token == Token::TypeAssign) {
		LOG_VERBOSE("Parsing TypeAssign");
		lexer->NextToken();	//Eat the type assignment operator and get the type token
		//If the next token provided by the lexer is not a identifier then the user is being stupid
		if(lexer->token != Token::Identifer) {
			LOG_ERROR(lexer->filePos << "Expected a type identifier after type assign opperator");
			return nullptr;
		}
		//The lexer has now tokenized the type try and see if it has been defined
		type = StringToType(lexer->tokenString);
		//The type has not been defined
		if(type == nullptr) {
			LOG_ERROR(lexer->filePos << "type(" << lexer->tokenString << ")is undefined!");
			return nullptr;
		}

		//The type was determined successfully!;
		LOG_INFO(identifierFilePos << " (" << name << ") of type(" << lexer->tokenString << ") declared!");
		return new ASTVariable(name, type);
	}

	//TYPE INFER
	else if(lexer->token == Token::TypeInfer) {
		//TODO type infrence
		return nullptr;
	}

	//TYPE DEFINE
	else if (lexer->token == Token::TypeDefine) {
		LOG_VERBOSE("Parsing TypeDefine");
		lexer->NextToken();

		//If the token is a openParen then this is a function definition
		//FUNCTION DEFINITION
		if(lexer->token == Token::ParenOpen) {
			return ParseFunction(name);
		}

		//The token is some identifier so this is a data structure definition... or something
		if(lexer->token == Token::Identifer) {
			//For now we will assume any custom data type being created must be a struct or something...
			//For not we will not handle this yet
			LOG_ERROR("Custom data types not implemented yet!");
			return nullptr;
		}
	}

	//FunctionCall!
	else if (lexer->token == Token::ParenOpen) {
		LOG_VERBOSE("Parsing Call to: " << name);

		std::vector<ASTExpression*> args;
		lexer->NextToken();
		while(lexer->token != Token::ParenClose && lexer->token != Token::Unkown && lexer->token != Token::EndOfFile) {
			lexer->NextToken();
		}

		if(lexer->token == Token::ParenClose) {
			lexer->NextToken();
		}


		ASTCall* call = new ASTCall(name, args);
		codeGenerator->Codegen(call);
		return call;
	}

	LOG_ERROR(lexer->filePos << "Unknown token after identifier '" << name << "' [ '" << lexer->tokenString << "' ]");
	return nullptr;
}

ASTNode* Parser::ParsePrimary() {
	lexer->NextToken();
	switch (lexer->token) {
	case Token::Identifer:
		return ParseIdentifier();
	case Token::Number:
		return ParseNumber();
	case Token::ScopeOpen:
		LOG_VERBOSE(lexer->filePos << "Parsing a new scope");
		break;
	case Token::EndOfFile:
		return nullptr;
	default:
		LOG_ERROR(lexer->filePos << "Unknown token when expecting expression");
		return nullptr;
	}
}

ASTNumber* Parser::ParseNumber() {
	return nullptr;
}

void Parser::ParseFile() {
	lexer->NextToken();
	while (lexer->token != Token::EndOfFile) {
		switch (lexer->token) {
		case Token::Identifer:
			ParseIdentifier();
			break;
		case Token::Number:
			ParseNumber();
			break;
		case Token::Foreign:
			break;
		case Token::ParenOpen:
			break;
		case Token::ParenClose:
			break;
		case Token::Unkown:
			LOG_ERROR(lexer->filePos << "Unknown Token(" << lexer->tokenString << ")");
			break;
		}
		lexer->NextToken();
	}


	if (llvm::verifyModule(*module))
		LOG_ERROR("LLVMModule verification failed!");

	LOG_INFO("Writing IR to file");
	module->dump();
}
