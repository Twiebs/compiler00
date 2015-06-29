#include "Parser.hpp"

Parser::Parser(llvm::Module* module, std::string filename, std::ifstream* stream) {
	this->module = module;
	auto lastSlash = filename.find_last_of('/');
	auto briefFilename = filename.substr(lastSlash + 1);

	lexer = new Lexer(briefFilename, stream);
	codeGenerator = new CodeGenerator(module);

	AST::InitalizeLanguagePrimitives(module);
	currentScope = AST::globalScope;

	precedenceMap[(int32)Token::ADD] = 20;
	precedenceMap[(int32)Token::SUB] = 20;
	precedenceMap[(int32)Token::MUL] = 40;
	precedenceMap[(int32)Token::DIV] = 40;
}

Parser::~Parser() {
	delete lexer;
	delete codeGenerator;
}

int32 Parser::GetCurrentTokenPrecedence() {
	auto result = precedenceMap[(int32)lexer->token];
	if(result <= 0) {
		//If the token is not an operator we return -1 because is it exempt from these types of things!
		return -1;
	}
	return result;
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
			auto typeIdentifier = AST::FindIdentifier(currentScope, lexer->tokenString);
			if (!typeIdentifier) {
				LOG_ERROR(lexer->filePos << "identifier(" << lexer->tokenString << ")is undefined!");
				return nullptr;
			} else if (typeIdentifier->node->nodeType != ASTNodeType::TypeDefinition) {
				LOG_ERROR(lexer->filePos << "identifier(" << lexer->tokenString << ")was declared but is not a type definition!");
				return nullptr;
			}

			//The type of the declaration has already been resolved
			auto type = (AST::TypeDefinition*) typeIdentifier->node;

			//We now need to ensure that this indentifier does not yet exist

			//We have gotten this far so we know that we are declaring a identifier of a type that has been resolved!
			auto ident = AST::FindIdentifier(currentScope, identifierName);
			if(ident != nullptr) {
				LOG_ERROR("Redefintion of identifier " << identifierName << " declared at " << ident->position);
				return nullptr;
			}

			ident = AST::CreateIdentifier(currentScope, identifierName);
			auto var = AST::CreateVariable();
			//Actually this is kind of fucked... Identifiers should only be able to point to expressions
			ident->node = var;	//TODO add an AssignIdentifierToNode() type of thing??
			var->identifier = ident;
			var->type = type;

			//Now we check to see if this identifier will be assigned a default value!
			//We need to save an expression that represents the value that this identifier will store because it might be a function expression that has not been resolved yet!
			lexer->NextToken();	//Eat the type
			if (lexer->token == Token::EQUALS) {
				lexer->NextToken();	//Eat the assignment operator
				var->initalExpression = ParseExpression();
				LOG_INFO(ident->position << "Identifier(" << ident->name << ") of Type(" << type->identifier->name << ") declared with an inital expression specified!");
			} else {
				LOG_INFO(ident->position << "Identifier(" << ident->name << ") of Type(" << type->identifier->name << ") declared!");
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
			auto identifier = AST::FindIdentifier(currentScope, identifierName);
			if (identifier != nullptr) {
				if(identifier->node == nullptr) {
					LOG_INFO("An idenfitier called: " << identifier->name << " was allready declared at " << identifier->position << " but has not been resolved!  We are now defining it!");
				} else {
					LOG_ERROR(lexer->filePos << "Identifier '" << identifierName << "' was already defined at" << identifier->position);
					return nullptr;   //The identifier has already been defined!  It can not be type assigned
				}
			} else {
				//The identifier was not already created so we are now able to create one!
				identifier = AST::CreateIdentifier(currentScope, identifierName);
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
				function->ident = identifier;
				currentScope = function;

				//PARSE FUNCTION DEFN ARGUMENTS!
				lexer->NextToken(); //Eat the open paren
				while (lexer->token != Token::ParenClose) {
					// If this is a function defn then it should only have decleartions in its argument lsit!
					// Its assumed parsePrimary will handle any EOF / unknowns
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

					AST::Identifier* returnTypeIdentifier = AST::FindIdentifier(currentScope, lexer->tokenString);
					if (returnTypeIdentifier == nullptr) {
						LOG_ERROR("...Unknown identifier");
						return nullptr;
					}

					function->returnType = (AST::TypeDefinition*) returnTypeIdentifier->node;
					lexer->NextToken();
				}

				//There was no type return ':>' operator after the argument list
				else {
					function->returnType = (AST::TypeDefinition*) (AST::FindIdentifier(currentScope, "Void")->node);
				}

				if (lexer->token == Token::ScopeOpen) {
					//A new scope has been opened...
					lexer->NextToken(); //Eat the scope

					//WAIT ALL OF THIS IS WRONG!
					//We need to codegen the function here!
					//But what if we cant?
					//There is no reason to not allready make the function here...
					//What if we have to resolve a dependency somewhere down the line and can not emit
					//Code for members?
					//Well... then we wait? and pick up from where we were? that might be find


					while (lexer->token != Token::ScopeClose && lexer->token != Token::EndOfFile) {

						AST::Node* node = ParseStatement();
						if (node == nullptr) {
							LOG_ERROR(lexer->filePos << " Could not generate code for statement inside function body: " << identifier->name);
							return nullptr;
						}
						function->members.push_back(node);
					}

				} else if (lexer->token != Token::Foreign) {
					LOG_ERROR("Expected a new scope to open '{' after function definition!");
					return nullptr;
				}

				lexer->NextToken();	//Eats the foreign or the end of the scope
				codeGenerator->Codegen(function);
				currentScope = function->parent;
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
			auto identifier = AST::FindIdentifier(currentScope, identifierName);
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
					LOG_ERROR(lexer->filePos << " Could not resolve expression for argument at index " << call->args.size() << " in call to function " << identifier->name);
					return nullptr;
				}
				call->args.push_back(expression);
			}
			lexer->NextToken();	//Eat the close ')'
			return call;
		}

		//Do a switch here!
		switch(lexer->token) {
		case Token::EQUALS:
		case Token::ADD_EQUALS:
		case Token::SUB_EQUALS:
		case Token::MUL_EQUALS:
		case Token::DIV_EQUALS:
		case Token::MOD_EQUALS:
 			lexer->NextToken();	//Eat the assignment operator!
			auto ident = AST::FindIdentifier(currentScope, identifierName);

			if(ident == nullptr) {
				LOG_ERROR("Could not assign a value to unknown variable " << identifierName);
				return nullptr;
			}

			auto var = (AST::Variable*)ident->node;
			if(var->nodeType != ASTNodeType::Variable) {
				LOG_ERROR("Recognized identifier " << identifierName << " but it is not a variable!");
			}

			auto expr = ParseExpression();
			if(expr == nullptr) {
				LOG_ERROR("Could not parse expression on the right of the assignment operator");
				return nullptr;
			}
			return AST::CreateVariableMutation(lexer->token, var, expr);
		}

		//We have gotten past all our routines
		//THIS SHOULD NEVER HAPPEN!
		LOG_ERROR(lexer->filePos << " Unknown token after identifier '" << identifierName << "' [ '" << lexer->tokenString << "' ]");
		return nullptr;
	}
		break;

	case Token::RETURN:
	{
		LOG_VERBOSE(lexer->filePos << " Parsing a return value");
		lexer->NextToken();	//Eat the return
		auto expr = ParseExpression();
		auto returnVal = AST::CreateReturnValue(expr);
		return returnVal;
	} break;
	default:
		LOG_ERROR(lexer->filePos << " Failed to parse Statement '" << lexer->tokenString << "', the statement did not begin with an identifier or a keyword!!!");
		lexer->NextToken();
		break;
	}
}

AST::Expression* Parser::ParseExpressionRHS(int32 exprPrec, AST::Expression* lhs) {
	while(true) {
		//If the token prec is less than 0 that means that this is not a binary opperator
		//And we dont have to do anything aside from returning the allready parsed expression
		auto tokenPrec = GetCurrentTokenPrecedence();
		if(tokenPrec < 0) {
			return lhs;
		}

		//We know that the currentToken is a binop
		auto binop = lexer->token;
		auto binopStr = lexer->tokenString;
		auto binopPos = lexer->filePos;
		lexer->NextToken();	//Eat the binop

		//We have a binop lets see what is on the other side!
		AST::Expression* rhs = ParsePrimaryExpression();
		if(rhs == nullptr) {
			LOG_ERROR(binopPos << " Could not parse primary expression to the right of binary opperator '" << binopStr << "'");
			return nullptr;
		}

		auto nextPrec = GetCurrentTokenPrecedence();
		if(tokenPrec < nextPrec) {
			rhs = ParseExpressionRHS(tokenPrec + 1, rhs);
			if(rhs == nullptr){
				LOG_ERROR("Could not parse recursive rhsParsing!");
				return nullptr;
			}
		}

		//@Memory there is a leak here!
		lhs = (AST::Expression*)AST::CreateBinaryOperation(binop, lhs, rhs);
	}	//Goes back to the while loop
}


AST::Expression* Parser::ParseExpression() {
	auto lhs = ParsePrimaryExpression();
	if(lhs == nullptr){
		return nullptr;
	}
	return ParseExpressionRHS(0, lhs);
}


//NOTE Parse Primary assumes the the lexer has already been incremented to the next token!
AST::Expression* Parser::ParsePrimaryExpression() {
	switch (lexer->token) {

	//Handles variables, function calls!
	case Token::Identifier:
	{
		LOG_VERBOSE("Parsing an identifier expression! for identifier -> " << lexer->tokenString);
		auto ident = AST::FindIdentifier(currentScope, lexer->tokenString);
		if(!ident) {
			LOG_ERROR(lexer->filePos << " identifier " << lexer->tokenString << " does not exist!");
			return nullptr;
		}

		lexer->NextToken(); //Consume the identifier
		//@Duplicate of procedure inside of ParseStatement!
		if(lexer->token == Token::ParenOpen) {
			LOG_VERBOSE("Parsing Call to: " << ident->name);
			AST::Call* call = AST::CreateCall();
			call->function = (AST::Function*) ident->node;

			lexer->NextToken();
			while (lexer->token != Token::ParenClose && lexer->token != Token::Unkown && lexer->token != Token::EndOfFile) {
				AST::Expression* expression = ParseExpression();
				if (expression == nullptr) {
					LOG_ERROR(lexer->filePos << " Could not resolve expression at argument index" << call->args.size() << "in call to function " << ident->name);
					return nullptr;
				}
				call->args.push_back(expression);
			}
			lexer->NextToken();	//Eat the close ')'
			return (AST::Expression*)call;
		}
		return (AST::Expression*)ident->node;
	}break;
	//This is a numeric literal!
	case Token::Number: {
		LOG_VERBOSE("Parsing a numberExpression!");
		auto isFloat = lexer->tokenString.find(".") != std::string::npos ? true : false;
		if (isFloat) {
			auto value = std::stof(lexer->tokenString);
			auto result = AST::CreateFloatLiteral(value);
			lexer->NextToken();	//Eats the float literal
			return result;
		} else {
			auto result = AST::CreateIntegerLiteral();
			result->intType = (AST::TypeDefinition*) (AST::FindIdentifier(currentScope, "S32")->node);
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
		LOG_ERROR("HIT END OF FILE! THIS IS TERRIBLE YOU ARE MENTALY DISABLED, USER!");
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

	std::cout << "\x1b[33m" << "\n";
	module->dump();
	LOG_INFO("Compilation Finished: There were errors");
}
