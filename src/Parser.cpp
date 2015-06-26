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



AST::TypeDefinition* Parser::StringToTypeDefinition(std::string string) {
	uint32 typeIndex =  namedTypeDefinitions[string];
	if(typeIndex == 0) {
		LOG_DEBUG(string << " has not been defined!");
		return nullptr;
	}
      AST::TypeDefinition* type = &typeDefinitions[typeIndex - 1];
	return type;
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

ASTFunction* Parser::ParseFunction(std::string& identifierName) {

}



ASTType* Parser::ParseType() {
	//Eat the previous token and get the

	lexer->NextToken();
	ASTType* type = StringToType(lexer->tokenString);
	return type;
}

AST::Node* Parser::ParseIdentifier() {
	LOG_VERBOSE("Parsing Identifier : " << lexer->tokenString);
      AST::Identifier* identifier = identifiers[lexer->tokenString];
      if(identifier == nullptr) {
            //TODO how are we going to handle AST allocations?
            //Store identifiers in an array contiguously? and point to the AST::Node they represent?
            //Does the node then point back to the identifier that represnts it?
            identifier = new AST::Identifier;
            identifier->position = lexer->filePos;
            identifier->name = lexer->tokenString;
            identifiers[lexer->tokenString] = identifier;
      }

      //We now have an identifier that has either been created or pulled from the idTable
	lexer->NextToken(); //Eat the identifier

	//TYPEASSIGNMENT
	if (lexer->token == Token::TypeAssign) {
		LOG_VERBOSE("Parsing TypeAssign");
		lexer->NextToken();	//Eat the type assignment operator and get the type token

            // If the next token provided by the lexer is not a identifier then the user is being stupid
            if(lexer->token != Token::Identifier) {
			LOG_ERROR(lexer->filePos << "Expected a type identifier after type assign opperator");
			return nullptr;
		}

            //The next token is an identifier we need to create a variable to represent it no mater what...
            //Actualy lets see what it is first...
            //Instead of looking up the type directly we can lookup the identifier and see if it points to a type...
            //This might be a good idea because it could provide better error messages and also removes ambiguity betweem functions, types
            //And other stuff.

            auto type = identifiers[lexer->tokenString];
            //TODO here we need to wait on identifiers/types to be resolved!1
            if (!type) {
                  LOG_ERROR(lexer->filePos << "type(" << lexer->tokenString << ")is undefined!");
			return nullptr;
            }

		//The type was determined successfully!;
        //TOOD stream opperator overload for identifiers?
		LOG_INFO(identifier->position << " (" << identifier->name << ") of type(" << lexer->tokenString << ") declared!");
		//What is a variable and why the fuck do i need one??  its never actualy used...
            //NOTE There was a ASTVariable being created and returned here... I deleted it and am returning a nullptr for now
            //Because this currently doesnt make a difference.
            //URGENT This probably is the source of your error dumb-dumb

            //TODO here we need to check if there is a n assignment opperator after the type

            //We need some type of memory manager that will create these variables and store them approraityl
            AST::Variable* variable = new AST::Variable;
            variable->identifier = identifier;
            variable->type = (AST::TypeDefinition*)identifier->node;
            return variable;
	}

	//TYPE INFER
	else if(lexer->token == Token::TypeInfer) {
		//TODO type infrence
		return nullptr;
	}

	//TYPE DEFINE
	else if (lexer->token == Token::TypeDefine) {
		LOG_VERBOSE("Parsing TypeDefine");
            //First we makesure that the identifier has not been resolved yet
            if(identifier->node != nullptr) {
                  LOG_ERROR(lexer->filePos << " Identifier " << identifier->name << "was allready defined at" << identifier->position);
                  return nullptr;   //The identifer has allready been defined!  It can not be type assigned
            }
            lexer->NextToken();

		//If the token is a openParen then this is a function definition
		//FUNCTION DEFINITION
		//TODO Parse Clojures here {}...
            //Move parse Function inline?
            //Thats probably a good idea because its only ever going to be called from right here anyway!


            if(lexer->token == Token::ParenOpen) {
            	LOG_VERBOSE("Parsing Function");
            	lexer->NextToken();    //Eat the open paren

            	AST::Function* function = new AST::Function;
            	function->identifier = identifier;

            	while(lexer->token != Token::ParenClose) {
            		//Its assumed parsePrimary will handle any EOF / unkowns
            		function->args.push_back(ParsePrimary());
            	}
            	//Eat the close ')'
            	lexer->NextToken();

            	if (lexer->token == Token::TypeReturn) {
            		lexer->NextToken();
            		if(lexer->token != Token::Identifier) {
            			LOG_ERROR("expected a type after the return operator");
            			return nullptr;
            		}

            		AST::Identifier* returnTypeIdentifier = identifiers[lexer->tokenString];
            		if(returnTypeIdentifier == nullptr) {
            			LOG_ERROR("...Unkown identifier");
            			return nullptr;
            		}

            		function->returnType = returnTypeIdentifier->node;
            		lexer->NextToken();

            	}

            	//There was no type return ':>' opperator after the argument list
            	else {
            		function->returnType = (AST::TypeDefinition*) (identifiers["Void"]->node);
            	}

            	//URGENT we might need to eat a token here?

            	if(lexer->token == Token::ScopeOpen) {
            		//A new scope has been opened...
            		while(lexer->token != Token::ScopeClose && lexer->token != Token::EndOfFile) {
            			AST::Node* node = ParsePrimary();
            			if(node == nullptr) {
            				LOG_ERROR("Could not generate code for statement inside function body: " << identifier->name);
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

		//The token is some identifier so this is a data structure definition... or something
		if(lexer->token == Token::Identifier) {
			//For now we will assume any custom data type being created must be a struct or something...
			//For not we will not handle this yet
			LOG_ERROR("Custom data types not implemented yet!");
			return nullptr;
		}
	}

	//FunctionCall!
	else if (lexer->token == Token::ParenOpen) {
		LOG_VERBOSE("Parsing Call to: " << identifier->name);

		AST::Call* call = new AST::Call;
		call->function = (AST::Function*)identifier->node;

		lexer->NextToken();
		while(lexer->token != Token::ParenClose && lexer->token != Token::Unkown && lexer->token != Token::EndOfFile) {
			lexer->NextToken();
		}

		if(lexer->token == Token::ParenClose) {
			lexer->NextToken();
		}
		codeGenerator->Codegen(call);
		return call;
	}

	LOG_ERROR(lexer->filePos << "Unknown token after identifier '" << identifier->name << "' [ '" << lexer->tokenString << "' ]");
	return nullptr;
}

AST::Node* Parser::ParsePrimary() {
	lexer->NextToken();
	switch (lexer->token) {
	case Token::Identifier:
		return ParseIdentifier();
	case Token::Number:
		LOG_ERROR(lexer->filePos << "ATTEMPTING TO PARSE A NUMBER!");
		return nullptr;
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


//URGENT(As soon as the compiler runs again resolve this!)
//What the fuck is this?
void Parser::ParseFile() {
	lexer->NextToken();
	while (lexer->token != Token::EndOfFile) {
		switch (lexer->token) {
		case Token::Identifier:
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
