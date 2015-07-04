#include "Parser.hpp"

Parser::Parser(llvm::Module* module, CodeGenerator* codeGenerator) {
	this->codeGenerator = codeGenerator;
	this->module = module;
	precedenceMap[(int32)Token::ADD] = 20;
	precedenceMap[(int32)Token::SUB] = 20;
	precedenceMap[(int32)Token::MUL] = 40;
	precedenceMap[(int32)Token::DIV] = 40;
}

Parser::~Parser() {
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
ASTNode* Parser::ParseStatement() {
	switch (lexer->token) {
	case Token::Identifier: {
		LOG_VERBOSE("Parsing Identifier : " << lexer->tokenString);
		//We can't assume we can just create an identifier here so we stash the identifiers name
		std::string identifierName = lexer->tokenString;
		lexer->NextToken(); //Eat the identifier

		// @TYPE_DECLARE
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
			auto typeIdentifier = FindIdentifier(currentScope, lexer->tokenString);
			if (!typeIdentifier) {
				LOG_ERROR(lexer->filePos << "identifier(" << lexer->tokenString << ")is undefined!");
				return nullptr;
			} else if (typeIdentifier->node->nodeType != AST_DEFINITION) {
				LOG_ERROR(lexer->filePos << "identifier(" << lexer->tokenString << ")was declared but is not a type definition!");
				return nullptr;
			}

			//The type of the declaration has already been resolved
			auto type = (ASTDefinition*) typeIdentifier->node;

			//We now need to ensure that this indentifier does not yet exist

			//We have gotten this far so we know that we are declaring a identifier of a type that has been resolved!
			auto ident = FindIdentifier(currentScope, identifierName);
			if(ident != nullptr) {
				LOG_ERROR("Redefintion of identifier " << identifierName << " declared at " << ident->position);
				return nullptr;
			}

			ident = CreateIdentifier(currentScope, identifierName);
			auto var = CreateVariable(currentScope);	//Add to scope!
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

		//@TYPE INFER
		else if (lexer->token == Token::TypeInfer) {
			//TODO type inference
			return nullptr;
		}

		//@TYPE DEFINE
		else if (lexer->token == Token::TypeDefine) {
			LOG_VERBOSE("Parsing TypeDefine");
			lexer->NextToken();


			//TODO Parse Closures here {}...
			//@FUNCTION DEFINITION
			if (lexer->token == Token::ParenOpen) {
				LOG_VERBOSE("Parsing FunctionDefinition");
				auto ident = FindIdentifier(currentScope, identifierName);
				if(ident == nullptr) {
					ident = CreateIdentifier(currentScope, identifierName);
					//@Memory - Unhandled heap allocation!
					auto funcSet = new ASTFunctionSet;
					ident->node = funcSet;
					funcSet->nodeType = AST_FUNCTION;	//NOTE function sets treated as functions!
					funcSet->ident = ident;
				} else if(ident->node == nullptr){
					LOG_ERROR("There is something screwy happeng in function defines!");
					LOG_INFO("Its probably because a variable was declared but not resolved of the same name as the function that the user is now defining!");
				}

				//We need to make sure that the current function with the given arguments does not yet exist within the function table!
				//For now assume the user is right!
				//NOTE ^bad philosiphy! the user is never right!
				//Note since we have already checked against the identifierTable we know this function has not yet been defined
				ASTFunction* function = CreateFunction(currentScope);
				//Create a function with that identifier and put it in the curretnScope;
				function->ident = ident;
				currentScope = function;

				//PARSE FUNCTION DEFN ARGUMENTS!
				lexer->NextToken(); //Eat the open paren
				while (lexer->token != Token::ParenClose) {
					// If this is a function defn then it should only have decleartions in its argument lsit!
					// Its assumed parsePrimary will handle any EOF / unknowns
					ASTNode* node = ParseStatement();
					//Function arguments are always declerations which are statements not expressions!
					if (node != nullptr) {
						if (node->nodeType != AST_VARIABLE) {
							LOG_ERROR("Function definition arguments must be variable declerations!");
							return nullptr;
						}
						auto var = (ASTVariable*) node;
						function->args.push_back(var);
					} else {
						LOG_ERROR(lexer->filePos << " Could not parse function arguments for funcion " << identifierName);
						return nullptr;
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

					ASTIdentifier* returnTypeIdentifier = FindIdentifier(currentScope, lexer->tokenString);
					if (returnTypeIdentifier == nullptr) {
						LOG_ERROR(lexer->filePos << " Unknown identifier when expecting a return type!");
						return nullptr;
					} else if (returnTypeIdentifier->node->nodeType != AST_DEFINITION) {
						LOG_ERROR(lexer->filePos << " Identifier '" << lexer->tokenString << "' is not a type! It was declared at " << returnTypeIdentifier->position);
					}

					function->returnType = (ASTDefinition*) returnTypeIdentifier->node;
					lexer->NextToken();
				}

				//There was no type return ':>' operator after the argument list
				else {
					function->returnType = (ASTDefinition*) (FindIdentifier(currentScope, "Void")->node);
				}

				auto FindFunction = [function](ASTIdentifier* ident) -> ASTFunction* {
					auto funcSet = (ASTFunctionSet*)ident->node;
					for(auto func : funcSet->functions) {
						bool functionsMatch = true;
						if(func->args.size() == function->args.size()) {
							for(uint32 i = 0; i < func->args.size(); i++) {
								if(func->args[i]->type != function->args[i]->type) {
									functionsMatch = false;
								}
							}
						} else functionsMatch = false;
						if(functionsMatch) {
							if(func->returnType != function->returnType) {
								LOG_ERROR("Cannot overload function return types!  Arguments must differ!");
								return nullptr;
							}
							return func;
						}
					}
					return nullptr;
				};

				auto func = FindFunction(ident);
				if(func != nullptr) {
					LOG_ERROR("Function re-definition!  Overloaded function " << identifierName << "was already defined!");
				} else {
					auto funcSet = (ASTFunctionSet*)ident->node;
					funcSet->functions.push_back(function);
				}

				if (lexer->token == Token::ScopeOpen) {
					//A new scope has been opened...
					lexer->NextToken(); //Eat the scope

					while (lexer->token != Token::ScopeClose && lexer->token != Token::EndOfFile) {
						ASTNode* node = ParseStatement();
						if (node == nullptr) {
							LOG_ERROR(lexer->filePos << " Could not parse statement inside function body: " << ident->name);
							return nullptr;
						}
						function->members.push_back(node);
					}

				} else if (lexer->token != Token::FOREIGN) {
					LOG_ERROR("Expected a new scope to open '{' after function definition!");
					LOG_INFO("Did you misspell foreign?");
					return nullptr;
				} else {
					if(function->parent->parent != nullptr) {
						//TODO why wouldn't we allow this?
						LOG_ERROR("Cannot create a foreign function nested in another block!  Foreign functions must be declared in the global scope!");
						return nullptr;
					}
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

		//@FunctionCall!
		else if (lexer->token == Token::ParenOpen) {
			LOG_VERBOSE("Attempting to parse call to : " << identifierName);
			auto ident = FindIdentifier(currentScope, identifierName);
			if (ident == nullptr) {
				LOG_ERROR("function named " << identifierName << " does not exist");
				return nullptr;
			}
			//Create the call and now determine its arguments
			ASTCall* call = CreateCall();
			call->ident = ident;

			lexer->NextToken(); //Eat the open Paren
			while (lexer->token != Token::ParenClose && lexer->token != Token::UNKOWN && lexer->token != Token::EndOfFile) {
				ASTExpression* expression = ParseExpression();
				if(expression == nullptr) {
					LOG_ERROR(lexer->filePos << " Could not resolve expression for argument at index " << call->args.size() << " in call to function " << ident->name);
					//DONT return here... just keep going so we can find more errors'
					continue;	//But we do skip pushing the expression on to the function arguments
					//it might be better to keep the nullptr so that we can determine the actually amount of arrugments that were specified by the user for better error reporting!
				}
				call->args.push_back(expression);
			}	//We push back all the arguments and don't care about what function we are actually going to end up calling...
				//We may or may not actually find the function that we are looking for!
			lexer->NextToken();	//Eat the close ')'


			auto funcSet = (ASTFunctionSet*)ident->node;
			if(funcSet->functions.size() == 0) {
				LOG_ERROR("There are no functions named " << identifierName);
				delete call;
				return nullptr;
			} else {
				for(auto func : funcSet->functions) {
					bool functionMatches = true;
					if(func->args.size() == call->args.size()) {
						for(uint32 i = 0; i < func->args.size(); i++) {
							if(func->args[i]->type != call->args[i]->type) {
								functionMatches = false;
							}
						}
					} else {
						functionMatches = false;
					}

					if(functionMatches) {
						call->function = func;
						break;
					}
				}

				if(call->function == nullptr) {
					LOG_ERROR("A function named " << ident->name << " exits but it does not take the provided arguments!");
					delete call;
					return nullptr;
				}
			}

			return call;
		}

		switch(lexer->token) {
		case Token::EQUALS:
		case Token::ADD_EQUALS:
		case Token::SUB_EQUALS:
		case Token::MUL_EQUALS:
		case Token::DIV_EQUALS:
		case Token::MOD_EQUALS:
 			lexer->NextToken();	//Eat the assignment operator!
			auto ident = FindIdentifier(currentScope, identifierName);

			if(ident == nullptr) {
				LOG_ERROR("Could not assign a value to unknown variable " << identifierName);
				return nullptr;
			}

			auto var = (ASTVariable*)ident->node;
			if(var->nodeType != AST_VARIABLE) {
				LOG_ERROR("Recognized identifier " << identifierName << " but it is not a variable!");
			}

			// TODO
			//This is actualy a good spot to return null because these are top level errors!
			//Make a new error called top level error that specifies actual errors in the program
			//Secondary errors might not be actual errors once you fix primary errors
			auto expr = ParseExpression();
			if(expr == nullptr) {
				LOG_ERROR("Could not parse expression on the right of the assignment operator");
				return nullptr;
			} else if (expr->type == nullptr) {
				LOG_ERROR("Could not determine type of expression to the right of " << var->identifier->name);
			}
				else if (expr->type != var->type) {
				LOG_ERROR("Type mismatch! Could not assign a value of '" << expr->type->identifier->name << "' to variable '" << var->identifier->name << "' of type '" << var->type->identifier->name << "' !");
				return nullptr;
			}
			return CreateMutation(lexer->token, var, expr);
		}

		//We have gotten past all our routines
		//THIS SHOULD NEVER HAPPEN!
		LOG_ERROR(lexer->filePos << " Unknown token after identifier '" << identifierName << "' [ '" << lexer->tokenString << "' ]");
		return nullptr;
	}
		break;

	case Token::IF:
	{
		LOG_VERBOSE(lexer->filePos << " Parsing an if statement!");
		lexer->NextToken();	//Eat the if

		auto expr = ParseExpression();
		if(!expr) {
			LOG_ERROR("Could not evaluate expression when parsing if statement!");
			return nullptr;
		}

		if(lexer->token == Token::ScopeOpen) {
			lexer->NextToken(); //Eat the open scope
			auto ifStatement = CreateIfStatement(expr);
			ifStatement->ifBlock = CreateBlock(currentScope);
			previousScope = currentScope;
			currentScope = ifStatement->ifBlock;
			while(lexer->token != Token::ScopeClose) {
				auto node = ParseStatement();
				if(!node) {
					LOG_ERROR("Could not parse statement inside of IF statement");
				} else {
					currentScope->members.push_back(node);
				}
			}

			currentScope = currentScope->parent;
			lexer->NextToken();	//Eat the '}'
			if(lexer->token == Token::ELSE) {
				LOG_VERBOSE("Parsing else statement");
				lexer->NextToken();	//Eat the else keyword!
				if(lexer->token == Token::ScopeOpen) {
					lexer->NextToken(); //Eat the '{'
					ifStatement->elseBlock = CreateBlock(currentScope);
					previousScope = currentScope;
					currentScope = ifStatement->elseBlock;
					while(lexer->token != Token::ScopeClose) {
						auto node = ParseStatement();
						if(!node) {
							LOG_ERROR("Could not parse statement inside of IF statement");
						} else {
							currentScope->members.push_back(node);
						}
					}
					currentScope = currentScope->parent;
					lexer->NextToken();	//Eat the '}'
				} else if (lexer->token == Token::IF){
					ifStatement->elseBlock = (ASTBlock*)ParseStatement();
				} else {
					LOG_ERROR("Expected a new scope to open after else keyword or the if keyword!! for else if statements dawggggggggggg!!!");
				}
			}
			return ifStatement;
		} else {
			LOG_ERROR("expected an open scope after if expression!");
			return nullptr;
		}

	} break;

	case Token::RETURN:
	{
		LOG_VERBOSE(lexer->filePos << " Parsing a return value");
		lexer->NextToken();	//Eat the return
		auto expr = ParseExpression();
		auto returnVal = CreateReturnValue(expr);
		return returnVal;
	} break;


	case Token::IMPORT:
		LOG_VERBOSE("Parsing an import statement!");
		lexer->NextToken();
		if(lexer->token != Token::Identifier) {
			LOG_ERROR("Exepcted an identifier after import statement, got '" << lexer->tokenString << "'");
			//Dont eat the token because its probably somthing that fell trough!
		} else {
			auto unit = parsedUnits[lexer->tokenString];
			if(unit == nullptr) {

			}
		}

		break;

	default:
		LOG_ERROR(lexer->filePos << " Failed to parse Statement '" << lexer->tokenString << "', the statement did not begin with an identifier or a keyword!!!");
		lexer->NextToken();
		break;
	}


}

ASTExpression* Parser::ParseExpressionRHS(int32 exprPrec, ASTExpression* lhs) {
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
		ASTExpression* rhs = ParsePrimaryExpression();
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

		if(lhs->type != rhs->type) {
			LOG_ERROR("Type mismatch! Can not convert '" << lhs->type->identifier->name << "' to " << rhs->type->identifier->name << "' !");
			//TODO return null here?
		}

		//@Memory there is a leak here!
		lhs = (ASTExpression*)CreateBinaryOperation(binop, lhs, rhs);
	}	//Goes back to the while loop
}


ASTExpression* Parser::ParseExpression() {
	auto lhs = ParsePrimaryExpression();
	if(lhs == nullptr){
		return nullptr;
	}
	return ParseExpressionRHS(0, lhs);
}


//NOTE Parse Primary assumes the the lexer has already been incremented to the next token!
ASTExpression* Parser::ParsePrimaryExpression() {
	switch (lexer->token) {

	//Handles variables, function calls!
	case Token::Identifier:
	{
		LOG_VERBOSE("Parsing an identifier expression! for identifier -> " << lexer->tokenString);
		auto ident = FindIdentifier(currentScope, lexer->tokenString);
		if(!ident) {
			LOG_ERROR(lexer->filePos << " identifier " << lexer->tokenString << " does not exist!");
			lexer->NextToken();
			return nullptr;
		}

		lexer->NextToken(); //Consume the identifier
		//@Duplicate of procedure inside of ParseStatement!
		if(lexer->token == Token::ParenOpen) {
			LOG_VERBOSE("Parsing Call to: " << ident->name);
			ASTCall* call = CreateCall();
			call->ident = ident;

			lexer->NextToken();
			while (lexer->token != Token::ParenClose && lexer->token != Token::UNKOWN && lexer->token != Token::EndOfFile) {
				ASTExpression* expression = ParseExpression();
				if (expression == nullptr) {
					LOG_ERROR(lexer->filePos << " Could not resolve expression at argument index" << call->args.size() << "in call to function " << ident->name);
					return nullptr;
				}
				call->args.push_back(expression);
			}
			lexer->NextToken();	//Eat the close ')'
			return (ASTExpression*)call;
		}
		return (ASTExpression*)ident->node;
	}break;
	//This is a numeric literal!
	case Token::Number: {
		LOG_VERBOSE("Parsing a numberExpression!");
		auto dotPos = lexer->tokenString.find(".");
		bool isFloat = dotPos == std::string::npos ? false : true;
		if (isFloat) {
			if(lexer->tokenString.substr(dotPos + 1).find(".") != std::string::npos) {
				LOG_ERROR("Floating Point value contains two decimal points!");
			}
			auto value = std::stof(lexer->tokenString);
			auto result = CreateFloatLiteral(value);
			lexer->NextToken();	//Eats the float literal
			return result;
		} else {
			auto value = std::stoi(lexer->tokenString);
			auto result = CreateIntegerLiteral(value);
			result->type = (ASTDefinition*) (FindIdentifier(currentScope, "S32")->node);
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

void Parser::ParseFile(std::string filename) {
	auto unit = parsedUnits[filename];
	assert(unit == nullptr);
	unit = new Unit;
	parsedUnits[filename] = unit;
	currentUnit = unit;
	currentScope = &unit->scope;
	//TODO
	//For now this is here until i get the parsing working again!
	InitalizeLanguagePrimitives(currentScope, module);

	auto oldLexer = lexer;
	lexer = new Lexer(filename);
	lexer->NextToken();	//Kicks of parsing... Grabs the first token in the file
	while (lexer->token != Token::EndOfFile) {
		ParseStatement();
	}
	delete lexer;
	if(oldLexer != nullptr) {
		lexer = oldLexer;
	}
}
