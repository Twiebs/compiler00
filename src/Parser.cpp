#include "Parser.hpp"

Parser::Parser(llvm::Module* module, std::string filename, std::ifstream* stream) {
	this->module = module;
	auto lastSlash = filename.find_last_of('/');
	auto briefFilename = filename.substr(lastSlash + 1);

	lexer = new Lexer(briefFilename, stream);
	codeGenerator = new CodeGenerator(module);

	AST::InitalizeLanguagePrimitives(module);

	operatorPrecedence[(int32) BinOp::ADD] = 20;
	operatorPrecedence[(int32) BinOp::SUB] = 20;
	operatorPrecedence[(int32) BinOp::MUL] = 40;
	operatorPrecedence[(int32) BinOp::DIV] = 40;
}

Parser::~Parser() {
	delete lexer;
	delete codeGenerator;
}

//NOTE Parse Primary assumes the the lexer has already been incremented to the next token!
AST::Node* Parser::ParseStatement() {
	switch (lexer->token) {
	//TODO this is where control flow keywords will live!

	case Token::Identifier: {
		LOG_VERBOSE("Parsing Identifier : " << lexer->tokenString);
		//We can't assume we can just create an identifier here so we stash the identifiers name
		std::string identifierName = lexer->tokenString;
		lexer->NextToken(); //Eat the identifier

		// TYPE_DECLARE
		if (lexer->token == Token::TypeDeclare) {
			LOG_VERBOSE("Parsing TypeDeclare");
			lexer->NextToken();	//Eat the type assignment operator and get the type token

			// If the next token provided by the lexer is not a identifier then the user is being stupid
			if (lexer->token != Token::Identifier) {
				LOG_ERROR(lexer->filePos << "Expected a type identifier after the Type Declare operator ':'");
				return nullptr;
			}

			//Now we check if the type the user is trying to assign has already been defined
			//TODO this is where depends resolving for type decls need to happen!
			auto typeIdentifier = AST::FindIdentifier(lexer->tokenString);
			if (!typeIdentifier) {
				LOG_ERROR(lexer->filePos << "identifier(" << lexer->tokenString << ")is undefined!");
				return nullptr;
			} else if (typeIdentifier->node->nodeType != ASTNodeType::TypeDefinition) {
				LOG_ERROR(lexer->filePos << "identifier(" << lexer->tokenString << ")was declared but is not a type definition!");
				return nullptr;
			}

			//The type of the declaration has already been resolved
			auto type = (AST::TypeDefinition*) typeIdentifier->node;

			//We have gotten this far so we know that we are declaring a identifier of a type that has been resolved!
			auto identifier = AST::CreateIdentifier(identifierName);
			auto var = AST::CreateVariable();
			//Actually this is kind of fucked... Identifiers should only be able to point to expressions
			identifier->node = var;	//TODO add an AssignIdentifierToNode() type of thing??
			var->identifier = identifier;
			var->type = type;

			//Now we check to see if this identifier will be assigned a default value!
			//We need to save an expression that represents the value that this identifier will store because it might be a function expression that has not been resolved yet!
			lexer->NextToken();	//Eat the type
			if (lexer->token == Token::AssignmentOpperator) {
				lexer->NextToken();	//Eat the assignment operator
				var->initalExpression = ParseExpression();
				LOG_INFO(identifier->position << " (" << identifier->name << ") of type(" << type->identifier->name << ") declared with an expression!");
			} else {
				LOG_INFO(identifier->position << " (" << identifier->name << ") of type(" << type->identifier->name << ") declared!");
			}
			return var;
		}

		//TYPE INFER
		else if (lexer->token == Token::TypeInfer) {
			//TODO type inference
			return nullptr;
		}

		//TYPE DEFINE
		else if (lexer->token == Token::TypeDefine) {
			LOG_VERBOSE("Parsing TypeDefine");
			//First we make sure that the identifier has not been resolved yet
			auto identifier = AST::FindIdentifier(identifierName);
			if (identifier != nullptr) {
				if(identifier->node == nullptr) {
					LOG_INFO("An idenfitier called: " << identifier->name << " was allready declared at " << identifier->position << " but has not been resolved!  We are now defining it!");
				} else {
					LOG_ERROR(lexer->filePos << "Identifier '" << identifierName << "' was already defined at" << identifier->position);
					return nullptr;   //The identifier has already been defined!  It can not be type assigned
				}
			} else {
				//The identifier was not already created so we are now able to create one!
				identifier = AST::CreateIdentifier(identifierName);
			}

			lexer->NextToken();
			//If the token is a openParen then this is a function definition
			//FUNCTION DEFINITION
			//TODO Parse Closures here {}...
			//Move parse Function inline?
			//Thats probably a good idea because its only ever going to be called from right here anyway!

			if (lexer->token == Token::ParenOpen) {
				LOG_VERBOSE("Parsing FunctionDefinition");
				//Note since we have already checked against the identifierTable we know this function has not yet been defined
				AST::Function* function = AST::CreateFunction();
				identifier->node = function;
				function->identifier = identifier;

				//PARSE FUNCTION DEFN ARGUMENTS!
				lexer->NextToken(); //Eat the open paren
				while (lexer->token != Token::ParenClose) {
					//If this is a function defn then it should only have decleartions in its argument lsit!
					//Its assumed parsePrimary will handle any EOF / unknowns
					AST::Node* node = ParseStatement();
					if (node != nullptr) {
						if (node->nodeType != ASTNodeType::Variable) {
							LOG_ERROR("Function definition arguments must be variables!");
							return nullptr;
						}
						auto var = (AST::Variable*) node;
						function->args.push_back(var);
					}
				}
				//Eat the close ')'
				lexer->NextToken();

				if (lexer->token == Token::TypeReturn) {
					lexer->NextToken();
					if (lexer->token != Token::Identifier) {
						LOG_ERROR("expected a type after the return operator");
						return nullptr;
					}

					AST::Identifier* returnTypeIdentifier = AST::FindIdentifier(lexer->tokenString);
					if (returnTypeIdentifier == nullptr) {
						LOG_ERROR("...Unknown identifier");
						return nullptr;
					}

					function->returnType = (AST::TypeDefinition*) returnTypeIdentifier->node;
					lexer->NextToken();

				}

				//There was no type return ':>' operator after the argument list
				else {
					function->returnType = (AST::TypeDefinition*) (AST::FindIdentifier("Void")->node);
				}

				if (lexer->token == Token::ScopeOpen) {
					//A new scope has been opened...
					lexer->NextToken(); //Eat the scope
					while (lexer->token != Token::ScopeClose && lexer->token != Token::EndOfFile) {

						AST::Node* node = ParseStatement();
						if (node == nullptr) {
							LOG_ERROR("Could not generate code for statement inside function body: " << identifier->name);
							return nullptr;
						}
						function->body.push_back(node);
					}

				} else if (lexer->token != Token::Foreign) {
					LOG_ERROR("Expected a new scope to open '{' after function definition!");
					return nullptr;
				}

				lexer->NextToken();	//Eats the foreign or the end of the scope
				codeGenerator->Codegen(function);
				return function;
			}

			//The token is some identifier so this is a data structure definition... or something
			if (lexer->token == Token::Identifier) {
				//For now we will assume any custom data type being created must be a struct or something...
				//For not we will not handle this yet
				LOG_ERROR("Custom data types not implemented yet!");
				return nullptr;
			}
		}

		//FunctionCall!
		else if (lexer->token == Token::ParenOpen) {
			LOG_VERBOSE("Parsing Call to: " << identifierName);
			auto identifier = AST::FindIdentifier(identifierName);
			if(identifier == nullptr) {
				LOG_ERROR("Function: " << identifierName << " does not exist");
				return nullptr;
			}

			AST::Call* call = AST::CreateCall();
			call->function = (AST::Function*) identifier->node;

			lexer->NextToken();
			while (lexer->token != Token::ParenClose && lexer->token != Token::Unkown && lexer->token != Token::EndOfFile) {
				AST::Expression* expression = ParseExpression();
				if(expression == nullptr) {
					LOG_ERROR("Unhandled expression");
					return nullptr;
				}
				call->args.push_back(expression);
			}
			lexer->NextToken();	//Eat the close ')'
			return call;
		}

		//We have gotten past all our routines
		//THIS SHOULD NEVER HAPPEN!
		LOG_ERROR(lexer->filePos << "Unknown token after identifier '" << identifierName << "' [ '" << lexer->tokenString << "' ]");
		return nullptr;
	}
		break;

	default:
		LOG_ERROR(lexer->filePos << "Error parsing statement '" << lexer->tokenString << "'! Statement did not begin with an identifier or a keyword!!!");
		lexer->NextToken();
		break;
	}
}

//NOTE Parse Primary assumes the the lexer has already been incremented to the next token!
AST::Expression* Parser::ParseExpression() {
	switch (lexer->token) {

	//Handles variables, function calls!
	case Token::Identifier:
	{
		LOG_VERBOSE("Parsing an identifier expression! for identifier -> " << lexer->tokenString);
		auto ident = AST::FindIdentifier(lexer->tokenString);
		if(!ident) {
			LOG_ERROR(lexer->filePos << " identifier " << lexer->tokenString << " does not exist!");
			return nullptr;
		}

		lexer->NextToken(); //Consume the identifier
		return (AST::Expression*)ident->node;
	}break;

	//This is a numeric literal!
	case Token::Number: {
		LOG_VERBOSE("Parsing a numberExpression!");
		auto isFloat = lexer->tokenString.find(".") != std::string::npos ? true : false;
		if (isFloat) {
			auto result = AST::CreateFloatLiteral();
			//TODO store language primitive types separately from user defined types for efficiency!
			result->floatType = (AST::TypeDefinition*) (AST::FindIdentifier("F32")->node);
			result->value = std::stof(lexer->tokenString);
			lexer->NextToken();	//Eats the float literal
			return result;
		} else {
			auto result = AST::CreateIntegerLiteral();
			result->intType = (AST::TypeDefinition*) (AST::FindIdentifier("S32")->node);
			result->value = std::stoi(lexer->tokenString);
			lexer->NextToken(); // Eats the integer literal
			return result;
		}
	}
		break;

	case Token::ScopeOpen:
		LOG_VERBOSE(lexer->filePos << "Parsing a new scope :: BADDDDDD!!!!!!");
		return nullptr;

	case Token::EndOfFile:
		LOG_ERROR("HIT END OF FILE! THIS IS TERRIABLE YOU ARE MENTALY DISABLED, USER!");
		return nullptr;
	default:
		LOG_ERROR(lexer->filePos << "Unknown token when expecting expression");
		lexer->NextToken(); //Increment the lexer to handle error recovery!
		return nullptr;
	}
	//This is dead code it will never happen.
	return nullptr;
}

//URGENT(As soon as the compiler runs again resolve this!)
//What the fuck is this?
void Parser::ParseFile() {
	lexer->NextToken();	//Kicks of parsing... Grabs the first token in the file
	while (lexer->token != Token::EndOfFile) {
		ParseStatement();
	}

	if (llvm::verifyModule(*module))
		LOG_ERROR("LLVMModule verification failed!");

	LOG_INFO("Writing IR to file");
	module->dump();
}
