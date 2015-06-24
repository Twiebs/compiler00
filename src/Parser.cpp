/*
 * Parser.cpp
 *
 *  Created on: Jun 18, 2015
 *      Author: torin
 */


#include "Parser.hpp"


Parser::Parser(llvm::Module* module, std::ifstream* stream) {
	this->module = module;
	lexer = new Lexer(stream);
	codeGenerator = new CodeGenerator(module);

	types.push_back(ASTType("Int32", (llvm::Type*)llvm::Type::getInt32Ty(module->getContext())));
	typeMap["Int32"] = 1;
}

Parser::~Parser() {
	delete lexer;
	delete codeGenerator;
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
	Token token = lexer->GetToken();
	//Loop through tokens until we find the end of the Paren
	std::vector<Type> args;
	while(token != Token::ParenClose) {
		//TODO care about function paramaters
	}

	if(token == Token::ParenClose) {
		lexer->GetToken();
	}

	//Here we handle if the function is an external function by simply setting that it is external if the function is
	//The main function other wise it is internal.. however this is sort of the opposite of what i acctualy want
	bool isForegin = (identiferName == "main") ? false : false;
	ASTPrototype* proto = new ASTPrototype(identiferName, args, isForegin);
	ASTExpression* body = ParseExpression();
	ASTFunction* function = new ASTFunction(proto, body);
	codeGenerator->Codegen(function);
	return function;
}



ASTType* Parser::ParseType() {
	//Eat the previous token and get the type
	lexer->GetToken();
	ASTType* type = StringToType(lexer->tokenString);
	return type;
}

ASTNode* Parser::ParseIdentifier() {
	LOG_VERBOSE("Parsing Identifier : " << lexer->tokenString);
	std::string name = lexer->tokenString;
	ASTType* type;

	//Eat the identifier and get the next token
	Token token = lexer->GetToken();

	//TYPEASSIGNMENT
	if (token == Token::TypeAssign) {
		LOG_VERBOSE("Parsing TypeAssign");
		token = lexer->GetToken();	//Eat the type assignment operator and get the type token
		//If the next token provided by the lexer is not a identifier then the user is being stupid
		if(token != Token::Identifer) {
			LOG_ERROR("Expected a type identifier after type assign opperator");
			return nullptr;
		}
		//The lexer has now tokenized the type try and see if it has been defined
		type = StringToType(lexer->tokenString);
		//The type has not been defined
		if(type == nullptr) {
			LOG_ERROR("type(" << lexer->tokenString << ")is undefined!");
			return nullptr;
		}

		//The type was determined successfully!;
		LOG_INFO("(" << name << ") of type(" << lexer->tokenString << ") declared! at line(" << lexer->lineNumber << ":" << lexer->colNumber << ")");
		return new ASTVariable(name, type);
	}

	//TYPE INFER
	else if(token == Token::TypeInfer) {
		//TODO type infrence
		return nullptr;
	}

	//TYPE DEFINE
	else if (token == Token::TypeDefine) {
		LOG_VERBOSE("Parsing TypeDefine");
		token = lexer->GetToken();

		//If the token is a openParen then this is a function definition
		//FUNCTION DEFINITION
		if(token == Token::ParenOpen) {
			return ParseFunction(name);
		}

		//The token is some identifier so this is a data structure definition... or something
		if(token == Token::Identifer) {
			//For now we will assume any custom data type being created must be a struct or something...
			//For not we will not handle this yet
			LOG_ERROR("Custom data types not implemented yet!");
			return nullptr;
		}
	}

	//FunctionCall!
	else if (token == Token::ParenOpen) {
		LOG_VERBOSE("Parsing Call to: " << name);

		std::vector<ASTExpression*> args;
		Token token = lexer->GetToken();
		while(token != Token::ParenClose && token != Token::Unkown && token != Token::EndOfFile) {
			token = lexer->GetToken();
		}

		if(token == Token::ParenClose) {
			lexer->GetToken();	//Eat the close
		}


		ASTCall* call = new ASTCall(name, args);
		codeGenerator->Codegen(call);
	}

	LOG_ERROR("Unknown token after identifier '" << name << "' (" << lexer->tokenString << ")");
	return nullptr;
}

ASTNode* Parser::ParsePrimary() {
	switch (lexer->GetToken()) {
	case Token::Identifer:
		return ParseIdentifier();
	case Token::Number:
		return ParseNumber();
	default:
		LOG_ERROR("Unknown token when expecting expression");
		return nullptr;
	}
}

ASTNumber* Parser::ParseNumber() {
	return nullptr;
}

void Parser::ParseFile() {
	Token token = lexer->GetToken();
	while (token != Token::EndOfFile) {
		switch (token) {
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
			LOG_ERROR(
					"Unknown Token(" << lexer->tokenString << ") at line(" << lexer->lineNumber << ":" << lexer->colNumber << ")");
			break;
		}
		token = lexer->GetToken();
	}


	if (llvm::verifyModule(*module))
		LOG_ERROR("LLVMModule verification failed!");

	LOG_INFO("Writing IR to file");
	module->dump();
}
