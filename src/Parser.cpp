#include "Parser.hpp"

//TODO a better way would be to store the precedences in a array
//and directly lookup the value without branching... especialy if the
//number of opperators begins to increase For now IF hax FTW!
//Operators and are compiling much larger projects to understand
// NOTE This should only be done after we have signifigantly more
//The Performance implications
// CONSIDER  moving to the AST since its not really a parsing thing?
int GetTokenPrecedence(Token token) {
	if (token == Token::ADD) return 20;
	if (token == Token::SUB) return 20;
	if (token == Token::MUL) return 40;
	if (token == Token::DIV) return 40;
	return -1;
}

Package* FindPackage(ParseState* state, const std::string& packageName) {
	for(auto package : state->packages) {
		if(package.name == packageName) return package;
	}
	return nullptr;
}


void ParseFile(const std::string& filename, BuildContext& context) {
	if (context.currentPackage != nullptr) {
		auto oldPackage = context.currentPackage;	// ptr cpy
		context.currentPackage = CreatePackage(filename);	// Ths needs to be a memory managment thing
		//The currentScope of the context has nothing to do with the build itself this is a parseState

		//The entire state of the context is copied out
		auto oldState = context.parseState;
		context.parseState  = {};	//The state is reset and we now set it to the new package

		//We ned to setput our new parseState for the currentPackage
		auto& state = context.parseState;
		state.stream.open(context.rootDir + filename);
		//TODO better error handling... We should set a flag that stops Codegen from happening
		if(!state.stream.is_open()) LOG_ERROR("Failed to open: " << filename);
		state.currentScope =  &currentPackage->scope; //Why should we need to do somthing like this?
		state.nextChar = state.stream.peek();
		state.lastChar = state.stream.get();	//I think doing some oop sutff here might be ok...
		while (state.token != Token::END_OF_FILE) {
			ParseStatement();
		}
	}

		//This fuckery is now irrelevant
		#if 0
		auto oldLexer = state.lexer;
		lexer = new Lexer(filename);
		NextToken(lex);	//Kicks of parsing... Grabs the first token in the file
		#endif
	}


	while (lex.token != Token::EndOfFile) {
		ParseStatement();
	}

	delete lexer;
	if(oldLexer != nullptr) {
		lexer = oldLexer;
	}

	if(oldPackage != nullptr) {
		currentPackage = oldPackage;
	}
}

//Does a parseContext make more sense in this case?
//The BuildContext should not be affected here
ASTNode* Parser::ParseStatement(ParseState& state) {
	auto lex = &state.lex;	//lol do this for now to see how it turns out
	Token token;
	switch (state.token) {
	case Token::IDENTIFIER: {
	//Statements should almost allways begin with an identifier
		//oh... thats interesting... Should we store information about the token in the
		//parseState but store the lexer book keeping inside of the lex state?
		//That seems very interesting
		LOG_VERBOSE("Parsing Identifier : " << lex.tokenString);
		//We can't assume we can just create an identifier here so we stash the identifiers name
		std::string identifierName = lex.tokenString;
		NextToken(lex);

		// @TYPE_DECLARE
		if (lex.token == Token::TypeDeclare) {
			LOG_VERBOSE("Parsing TypeDeclare");
			NextToken(lex);
			// If the next token provided by the lexer is not a identifier then the user is being stupid
			if (lex.token != Token::IDENTIFIER) {
				LOG_ERROR(lex.tokenSite << "Expected a type identifier after the Type Declare operator ':'");
				return nullptr;
			}

			//Now we check if the type the user is trying to assign has already been defined
			//TODO this is where depends resolving for type decls need to happen!
			auto typeIdentifier = FindIdentifierInScope(currentPackage, currentScope, lex.tokenString);
			if (!typeIdentifier) {
				LOG_ERROR(lex.tokenSite << "identifier(" << lex.tokenString << ")is undefined!");
				return nullptr;
			} else if (typeIdentifier->node->nodeType != AST_DEFINITION) {
				LOG_ERROR(lex.tokenSite << "identifier(" << lex.tokenString << ")was declared but is not a type definition!");
				return nullptr;
			}

			//The type of the declaration has already been resolved
			auto type = (ASTDefinition*) typeIdentifier->node;

			//We now need to ensure that this indentifier does not yet exist

			//We have gotten this far so we know that we are declaring a identifier of a type that has been resolved!
			auto ident = FindIdentifierInScope(currentPackage, currentScope, identifierName);
			if(ident != nullptr) {
				LOG_ERROR("Redefintion of identifier " << identifierName << " declared at " << ident->position);
				return nullptr;
			}

			ident = CreateIdentifier(currentScope, identifierName);
			ident->position = lex.tokenSite;
			auto var = CreateVariable(currentScope);	//Add to scope!
			ident->node = var;	//TODO add an AssignIdentifierToNode() type of thing??
			var->identifier = ident;
			var->type = type;

			//Now we check to see if this identifier will be assigned a default value!
			//We need to save an expression that represents the value that this identifier will store because it might be a function expression that has not been resolved yet!
			NextToken(lex);	//Eat the type
			if (lex.token == Token::EQUALS) {
				NextToken(lex);	//Eat the assignment operator
				var->initalExpression = ParseExpression();
				LOG_INFO(ident->position << "Identifier(" << ident->name << ") of Type(" << type->identifier->name << ") declared with an inital expression specified!");
			} else {
				LOG_INFO(ident->position << "Identifier(" << ident->name << ") of Type(" << type->identifier->name << ") declared!");
			}
			return var;
		}

		// NOTE @TYPE INFER
		else if (lex.token == Token::TypeInfer) {
			//TODO type inference
			return nullptr;
		}

		// NOTE @TYPE DEFINE
		else if (lex.token == Token::TypeDefine) {
			LOG_VERBOSE("Parsing TypeDefine");
			NextToken(lex);


			//TODO Parse Closures here {}...
			//@FUNCTION DEFINITION
			if (lex.token == Token::ParenOpen) {
				LOG_VERBOSE("Parsing FunctionDefinition");
				auto ident = FindIdentifierInScope(currentPackage, currentScope, identifierName);
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
				// NOTE ^bad philosiphy! the user is never right!
				// Note since we have already checked against the identifierTable we know this function has not yet been defined
				ASTFunction* function = CreateFunction(currentScope);
				//Create a function with that identifier and put it in the curretnScope;
				function->ident = ident;
				currentScope = function;

				//PARSE FUNCTION DEFN ARGUMENTS!
				NextToken(lex); //Eat the open paren
				while (lex.token != Token::ParenClose) {
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
						LOG_ERROR(lex.tokenSite << " Could not parse function arguments for funcion " << identifierName);
						return nullptr;
					}
				}
				//Eat the close ')'
				NextToken(lex);

				if (lex.token == Token::TypeReturn) {
					NextToken(lex);
					if (lex.token != Token::IDENTIFIER) {
						LOG_ERROR("expected a type after the return operator");
						return nullptr;
					}

					ASTIdentifier* returnTypeIdentifier = FindIdentifierInScope(currentPackage, currentScope, lex.tokenString);
					if (returnTypeIdentifier == nullptr) {
						LOG_ERROR(lex.tokenSite << " Unknown identifier when expecting a return type!");
						return nullptr;
					} else if (returnTypeIdentifier->node->nodeType != AST_DEFINITION) {
						LOG_ERROR(lex.tokenSite << " Identifier '" << lex.tokenString << "' is not a type! It was declared at " << returnTypeIdentifier->position);
					}

					function->returnType = (ASTDefinition*) returnTypeIdentifier->node;
					NextToken(lex);
				}

				//There was no type return ':>' operator after the argument list
				else {
					function->returnType = (ASTDefinition*) (FindIdentifierInScope(currentPackage, currentScope, "Void")->node);
				}

				auto FindFunction = [function](ASTIdentifier* ident) -> ASTFunction* {
					auto funcSet = (ASTFunctionSet*)ident->node;
					for(auto func : funcSet->functions) {
						bool functionsMatch = true;
						if(func->args.size() == function->args.size()) {
							for(U32 i = 0; i < func->args.size(); i++) {
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

				if (lex.token == Token::ScopeOpen) {
					//A new scope has been opened...
					NextToken(lex); //Eat the scope

					while (lex.token != Token::ScopeClose && lex.token != Token::EndOfFile) {
						ASTNode* node = ParseStatement();
						if (node == nullptr) {
							LOG_ERROR(lex.tokenSite << " Could not parse statement inside function body: " << ident->name);
							return nullptr;
						}
						function->members.push_back(node);
					}

				} else if (lex.token != Token::FOREIGN) {
					LOG_ERROR("Expected a new scope to open '{' after function definition!");
					LOG_INFO("Did you misspell foreign?");
					return nullptr;
				} else {
					if(function->parent->parent != nullptr) {
						// TODO why wouldn't we allow this?
						LOG_ERROR("Cannot create a foreign function nested in another block!  Foreign functions must be declared in the global scope!");
						return nullptr;
					}
				}

				NextToken(lex);	//Eats the foreign or the end of the scope
				codeGenerator->Codegen(function);
				currentScope = function->parent;
				return function;
			}

			//The token is some identifier so this is a data structure definition... or something
			if (lex.token == Token::IDENTIFIER) {
				//For now we will assume any custom data type being created must be a struct or something...
				//For not we will not handle this yet
				LOG_ERROR("Custom data types not implemented yet!");
				return nullptr;
			}
		}

		//@FunctionCall!
		else if (lex.token == Token::ParenOpen) {
			LOG_VERBOSE("Attempting to parse call to : " << identifierName);
			auto ident = FindIdentifierInScope(currentPackage, currentScope, identifierName);
			if (ident == nullptr) {
				LOG_ERROR("function named " << identifierName << " does not exist");
				return nullptr;
			}
			//Create the call and now determine its arguments
			ASTCall* call = CreateCall();
			call->ident = ident;

			NextToken(lex); //Eat the open Paren
			while (lex.token != Token::ParenClose && lex.token != Token::UNKOWN && lex.token != Token::EndOfFile) {
				ASTExpression* expression = ParseExpression();
				if(expression == nullptr) {
					LOG_ERROR(lex.tokenSite << " Could not resolve expression for argument at index " << call->args.size() << " in call to function " << ident->name);
					//DONT return here... just keep going so we can find more errors'
					continue;	//But we do skip pushing the expression on to the function arguments
					//it might be better to keep the nullptr so that we can determine the actually amount of arrugments that were specified by the user for better error reporting!
				}
				call->args.push_back(expression);
			}	//We push back all the arguments and don't care about what function we are actually going to end up calling...
				//We may or may not actually find the function that we are looking for!
			NextToken(lex);	//Eat the close ')'


			auto funcSet = (ASTFunctionSet*)ident->node;
			if(funcSet->functions.size() == 0) {
				LOG_ERROR("There are no functions named " << identifierName);
				delete call;
				return nullptr;
			} else {
				for(auto func : funcSet->functions) {
					bool functionMatches = true;
					if(func->args.size() == call->args.size()) {
						for(U32 i = 0; i < func->args.size(); i++) {
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

		//This is stuff that is done after the identifier
		switch(lex.token) {
		case Token::EQUALS:
		case Token::ADD_EQUALS:
		case Token::SUB_EQUALS:
		case Token::MUL_EQUALS:
		case Token::DIV_EQUALS:
		case Token::MOD_EQUALS:
 			NextToken(lex);	//Eat the assignment operator!
			auto ident = FindIdentifierInScope(currentPackage, currentScope, identifierName);

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
			return CreateMutation(lex.token, var, expr);
		}

		//We have gotten past all our routines
		//THIS SHOULD NEVER HAPPEN!
		LOG_ERROR(lex.tokenSite << " Unknown token after identifier '" << identifierName << "' [ '" << lex.tokenString << "' ]");
		return nullptr;
	}
		break;

	// NOTE @IF
	// If our statement does not start with an identifeir it must be a keyword
	case Token::IF:
	{
		LOG_VERBOSE(lex.tokenSite << " Parsing an if statement!");
		NextToken(lex);	//Eat the if

		auto expr = ParseExpression();
		if(!expr) {
			LOG_ERROR("Could not evaluate expression when parsing if statement!");
			return nullptr;
		}

		if(lex.token == Token::ScopeOpen) {
			NextToken(lex); //Eat the open scope
			auto ifStatement = CreateIfStatement(expr);
			ifStatement->ifBlock = CreateBlock(currentScope);
			previousScope = currentScope;
			currentScope = ifStatement->ifBlock;
			while(lex.token != Token::ScopeClose) {
				auto node = ParseStatement();
				if(!node) {
					LOG_ERROR("Could not parse statement inside of IF statement");
				} else {
					currentScope->members.push_back(node);
				}
			}

			currentScope = currentScope->parent;
			NextToken(lex);	//Eat the '}'
			if(lex.token == Token::ELSE) {
				LOG_VERBOSE("Parsing else statement");
				NextToken(lex);	//Eat the else keyword!
				if(lex.token == Token::ScopeOpen) {
					NextToken(lex); //Eat the '{'
					ifStatement->elseBlock = CreateBlock(currentScope);
					previousScope = currentScope;
					currentScope = ifStatement->elseBlock;
					while(lex.token != Token::ScopeClose) {
						auto node = ParseStatement();
						if(!node) {
							LOG_ERROR("Could not parse statement inside of IF statement");
						} else {
							currentScope->members.push_back(node);
						}
					}
					currentScope = currentScope->parent;
					NextToken(lex);	//Eat the '}'
				} else if (lex.token == Token::IF){
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
		LOG_VERBOSE(lex.tokenSite << " Parsing a return value");
		NextToken(lex);	//Eat the return
		auto expr = ParseExpression();
		auto returnVal = CreateReturnValue(expr);
		return returnVal;
	} break;


	case Token::IMPORT:
		LOG_VERBOSE("Parsing an import statement!");
		NextToken(lex);
		if(lex.token != Token::STRING) {
			LOG_ERROR("Exepcted a string after import statement, got '" << lex.tokenString << "'");
			//Dont eat the token because its probably somthing that fell trough!
		} else {
			auto package = parsedPackages[lex.tokenString];
			if(package == nullptr) {
				ParseFile(lex.tokenString);
			}

			currentPackage->importedPackages.push_back(lex.tokenString);
			NextToken(lex);	//Eat the string!
		}
		break;

	case Token::FOR:
		LOG_VERBOSE("Parsing a for statement");
		NextToken(lex);
		if(lex.token != Token::IDENTIFIER) {
			LOG_ERROR("Expected an identifier after for expression");
			return nullptr;
		}




		break;

	default:
		LOG_ERROR(lex.tokenSite << " Failed to parse Statement '" << lex.tokenString << "', the statement did not begin with an identifier or a keyword!!!");
		NextToken(lex);
		break;
	}
}

Parser::Parser(std::vector<std::string> importDirectories, llvm::Module* module, CodeGenerator* codeGenerator) {
	this->importDirectories = importDirectories;
	this->codeGenerator = codeGenerator;
	this->module = module;

	precedenceMap[(S32)Token::SUB] = 20;
	precedenceMap[(S32)Token::ADD] = 20;
	precedenceMap[(S32)Token::MUL] = 40;
	precedenceMap[(S32)Token::DIV] = 40;
	//Why the fuck bother with somthing like this?
	//When get Token Precedence is being called anyway!!!!

	primitivePackage = new Package;
	parsedPackages["primitives"] = primitivePackage;

	typeVoid = CreateType(&primitivePackage->scope, "Void", llvm::Type::getVoidTy(module->getContext()));

	CreateType(&primitivePackage->scope, "S8", llvm::Type::getInt8Ty(module->getContext()));
	CreateType(&primitivePackage->scope, "S16", llvm::Type::getInt16Ty(module->getContext()));
	typeS32 = CreateType(&primitivePackage->scope, "S32", llvm::Type::getInt32Ty(module->getContext()));
	CreateType(&primitivePackage->scope, "S64", llvm::Type::getInt64Ty(module->getContext()));

	CreateType(&primitivePackage->scope, "F16", llvm::Type::getHalfTy(module->getContext()));
	typeF32 = CreateType(&primitivePackage->scope, "F32", llvm::Type::getFloatTy(module->getContext()));
	CreateType(&primitivePackage->scope, "F64", llvm::Type::getDoubleTy(module->getContext()));
	CreateType(&primitivePackage->scope, "F128", llvm::Type::getFP128Ty(module->getContext()));
}

Parser::~Parser() {
	delete primitivePackage;
}



//NOTE Parse Primary assumes the the lexer has already been incremented to the next token!
ASTNode* Parser::ParseStatement() {
	switch (lex.token) {
	//Statements should almost allways begin with an identifier
	case Token::IDENTIFIER: {
		LOG_VERBOSE("Parsing Identifier : " << lex.tokenString);
		//We can't assume we can just create an identifier here so we stash the identifiers name
		std::string identifierName = lex.tokenString;
		NextToken(lex); //Eat the identifier

		// @TYPE_DECLARE
		if (lex.token == Token::TypeDeclare) {
			LOG_VERBOSE("Parsing TypeDeclare");
			NextToken(lex);	//Eat the type assignment operator and get the type token

			// If the next token provided by the lexer is not a identifier then the user is being stupid
			if (lex.token != Token::IDENTIFIER) {
				LOG_ERROR(lex.tokenSite << "Expected a type identifier after the Type Declare operator ':'");
				return nullptr;
			}

			//Now we check if the type the user is trying to assign has already been defined
			//TODO this is where depends resolving for type decls need to happen!
			auto typeIdentifier = FindIdentifierInScope(currentPackage, currentScope, lex.tokenString);
			if (!typeIdentifier) {
				LOG_ERROR(lex.tokenSite << "identifier(" << lex.tokenString << ")is undefined!");
				return nullptr;
			} else if (typeIdentifier->node->nodeType != AST_DEFINITION) {
				LOG_ERROR(lex.tokenSite << "identifier(" << lex.tokenString << ")was declared but is not a type definition!");
				return nullptr;
			}

			//The type of the declaration has already been resolved
			auto type = (ASTDefinition*) typeIdentifier->node;

			//We now need to ensure that this indentifier does not yet exist

			//We have gotten this far so we know that we are declaring a identifier of a type that has been resolved!
			auto ident = FindIdentifierInScope(currentPackage, currentScope, identifierName);
			if(ident != nullptr) {
				LOG_ERROR("Redefintion of identifier " << identifierName << " declared at " << ident->position);
				return nullptr;
			}

			ident = CreateIdentifier(currentScope, identifierName);
			ident->position = lex.tokenSite;
			auto var = CreateVariable(currentScope);	//Add to scope!
			ident->node = var;	//TODO add an AssignIdentifierToNode() type of thing??
			var->identifier = ident;
			var->type = type;

			//Now we check to see if this identifier will be assigned a default value!
			//We need to save an expression that represents the value that this identifier will store because it might be a function expression that has not been resolved yet!
			NextToken(lex);	//Eat the type
			if (lex.token == Token::EQUALS) {
				NextToken(lex);	//Eat the assignment operator
				var->initalExpression = ParseExpression();
				LOG_INFO(ident->position << "Identifier(" << ident->name << ") of Type(" << type->identifier->name << ") declared with an inital expression specified!");
			} else {
				LOG_INFO(ident->position << "Identifier(" << ident->name << ") of Type(" << type->identifier->name << ") declared!");
			}
			return var;
		}

		// NOTE @TYPE INFER
		else if (lex.token == Token::TypeInfer) {
			//TODO type inference
			return nullptr;
		}

		// NOTE @TYPE DEFINE
		else if (lex.token == Token::TypeDefine) {
			LOG_VERBOSE("Parsing TypeDefine");
			NextToken(lex);


			//TODO Parse Closures here {}...
			//@FUNCTION DEFINITION
			if (lex.token == Token::ParenOpen) {
				LOG_VERBOSE("Parsing FunctionDefinition");
				auto ident = FindIdentifierInScope(currentPackage, currentScope, identifierName);
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
				// NOTE ^bad philosiphy! the user is never right!
				// Note since we have already checked against the identifierTable we know this function has not yet been defined
				ASTFunction* function = CreateFunction(currentScope);
				//Create a function with that identifier and put it in the curretnScope;
				function->ident = ident;
				currentScope = function;

				//PARSE FUNCTION DEFN ARGUMENTS!
				NextToken(lex); //Eat the open paren
				while (lex.token != Token::ParenClose) {
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
						LOG_ERROR(lex.tokenSite << " Could not parse function arguments for funcion " << identifierName);
						return nullptr;
					}
				}
				//Eat the close ')'
				NextToken(lex);

				if (lex.token == Token::TypeReturn) {
					NextToken(lex);
					if (lex.token != Token::IDENTIFIER) {
						LOG_ERROR("expected a type after the return operator");
						return nullptr;
					}

					ASTIdentifier* returnTypeIdentifier = FindIdentifierInScope(currentPackage, currentScope, lex.tokenString);
					if (returnTypeIdentifier == nullptr) {
						LOG_ERROR(lex.tokenSite << " Unknown identifier when expecting a return type!");
						return nullptr;
					} else if (returnTypeIdentifier->node->nodeType != AST_DEFINITION) {
						LOG_ERROR(lex.tokenSite << " Identifier '" << lex.tokenString << "' is not a type! It was declared at " << returnTypeIdentifier->position);
					}

					function->returnType = (ASTDefinition*) returnTypeIdentifier->node;
					NextToken(lex);
				}

				//There was no type return ':>' operator after the argument list
				else {
					function->returnType = (ASTDefinition*) (FindIdentifierInScope(currentPackage, currentScope, "Void")->node);
				}

				auto FindFunction = [function](ASTIdentifier* ident) -> ASTFunction* {
					auto funcSet = (ASTFunctionSet*)ident->node;
					for(auto func : funcSet->functions) {
						bool functionsMatch = true;
						if(func->args.size() == function->args.size()) {
							for(U32 i = 0; i < func->args.size(); i++) {
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

				if (lex.token == Token::ScopeOpen) {
					//A new scope has been opened...
					NextToken(lex); //Eat the scope

					while (lex.token != Token::ScopeClose && lex.token != Token::EndOfFile) {
						ASTNode* node = ParseStatement();
						if (node == nullptr) {
							LOG_ERROR(lex.tokenSite << " Could not parse statement inside function body: " << ident->name);
							return nullptr;
						}
						function->members.push_back(node);
					}

				} else if (lex.token != Token::FOREIGN) {
					LOG_ERROR("Expected a new scope to open '{' after function definition!");
					LOG_INFO("Did you misspell foreign?");
					return nullptr;
				} else {
					if(function->parent->parent != nullptr) {
						// TODO why wouldn't we allow this?
						LOG_ERROR("Cannot create a foreign function nested in another block!  Foreign functions must be declared in the global scope!");
						return nullptr;
					}
				}

				NextToken(lex);	//Eats the foreign or the end of the scope
				codeGenerator->Codegen(function);
				currentScope = function->parent;
				return function;
			}

			//The token is some identifier so this is a data structure definition... or something
			if (lex.token == Token::IDENTIFIER) {
				//For now we will assume any custom data type being created must be a struct or something...
				//For not we will not handle this yet
				LOG_ERROR("Custom data types not implemented yet!");
				return nullptr;
			}
		}

		//@FunctionCall!
		else if (lex.token == Token::ParenOpen) {
			LOG_VERBOSE("Attempting to parse call to : " << identifierName);
			auto ident = FindIdentifierInScope(currentPackage, currentScope, identifierName);
			if (ident == nullptr) {
				LOG_ERROR("function named " << identifierName << " does not exist");
				return nullptr;
			}
			//Create the call and now determine its arguments
			ASTCall* call = CreateCall();
			call->ident = ident;

			NextToken(lex); //Eat the open Paren
			while (lex.token != Token::ParenClose && lex.token != Token::UNKOWN && lex.token != Token::EndOfFile) {
				ASTExpression* expression = ParseExpression();
				if(expression == nullptr) {
					LOG_ERROR(lex.tokenSite << " Could not resolve expression for argument at index " << call->args.size() << " in call to function " << ident->name);
					//DONT return here... just keep going so we can find more errors'
					continue;	//But we do skip pushing the expression on to the function arguments
					//it might be better to keep the nullptr so that we can determine the actually amount of arrugments that were specified by the user for better error reporting!
				}
				call->args.push_back(expression);
			}	//We push back all the arguments and don't care about what function we are actually going to end up calling...
				//We may or may not actually find the function that we are looking for!
			NextToken(lex);	//Eat the close ')'


			auto funcSet = (ASTFunctionSet*)ident->node;
			if(funcSet->functions.size() == 0) {
				LOG_ERROR("There are no functions named " << identifierName);
				delete call;
				return nullptr;
			} else {
				for(auto func : funcSet->functions) {
					bool functionMatches = true;
					if(func->args.size() == call->args.size()) {
						for(U32 i = 0; i < func->args.size(); i++) {
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

		//This is stuff that is done after the identifier
		switch(lex.token) {
		case Token::EQUALS:
		case Token::ADD_EQUALS:
		case Token::SUB_EQUALS:
		case Token::MUL_EQUALS:
		case Token::DIV_EQUALS:
		case Token::MOD_EQUALS:
 			NextToken(lex);	//Eat the assignment operator!
			auto ident = FindIdentifierInScope(currentPackage, currentScope, identifierName);

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
			return CreateMutation(lex.token, var, expr);
		}

		//We have gotten past all our routines
		//THIS SHOULD NEVER HAPPEN!
		LOG_ERROR(lex.tokenSite << " Unknown token after identifier '" << identifierName << "' [ '" << lex.tokenString << "' ]");
		return nullptr;
	}
		break;

	// NOTE @IF
	// If our statement does not start with an identifeir it must be a keyword
	case Token::IF:
	{
		LOG_VERBOSE(lex.tokenSite << " Parsing an if statement!");
		NextToken(lex);	//Eat the if

		auto expr = ParseExpression();
		if(!expr) {
			LOG_ERROR("Could not evaluate expression when parsing if statement!");
			return nullptr;
		}

		if(lex.token == Token::ScopeOpen) {
			NextToken(lex); //Eat the open scope
			auto ifStatement = CreateIfStatement(expr);
			ifStatement->ifBlock = CreateBlock(currentScope);
			previousScope = currentScope;
			currentScope = ifStatement->ifBlock;
			while(lex.token != Token::ScopeClose) {
				auto node = ParseStatement();
				if(!node) {
					LOG_ERROR("Could not parse statement inside of IF statement");
				} else {
					currentScope->members.push_back(node);
				}
			}

			currentScope = currentScope->parent;
			NextToken(lex);	//Eat the '}'
			if(lex.token == Token::ELSE) {
				LOG_VERBOSE("Parsing else statement");
				NextToken(lex);	//Eat the else keyword!
				if(lex.token == Token::ScopeOpen) {
					NextToken(lex); //Eat the '{'
					ifStatement->elseBlock = CreateBlock(currentScope);
					previousScope = currentScope;
					currentScope = ifStatement->elseBlock;
					while(lex.token != Token::ScopeClose) {
						auto node = ParseStatement();
						if(!node) {
							LOG_ERROR("Could not parse statement inside of IF statement");
						} else {
							currentScope->members.push_back(node);
						}
					}
					currentScope = currentScope->parent;
					NextToken(lex);	//Eat the '}'
				} else if (lex.token == Token::IF){
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
		LOG_VERBOSE(lex.tokenSite << " Parsing a return value");
		NextToken(lex);	//Eat the return
		auto expr = ParseExpression();
		auto returnVal = CreateReturnValue(expr);
		return returnVal;
	} break;


	case Token::IMPORT:
		LOG_VERBOSE("Parsing an import statement!");
		NextToken(lex);
		if(lex.token != Token::STRING) {
			LOG_ERROR("Exepcted a string after import statement, got '" << lex.tokenString << "'");
			//Dont eat the token because its probably somthing that fell trough!
		} else {
			auto package = parsedPackages[lex.tokenString];
			if(package == nullptr) {
				ParseFile(lex.tokenString);
			}

			currentPackage->importedPackages.push_back(lex.tokenString);
			NextToken(lex);	//Eat the string!
		}
		break;

	case Token::FOR:
		LOG_VERBOSE("Parsing a for statement");
		NextToken(lex);
		if(lex.token != Token::IDENTIFIER) {
			LOG_ERROR("Expected an identifier after for expression");
			return nullptr;
		}




		break;

	default:
		LOG_ERROR(lex.tokenSite << " Failed to parse Statement '" << lex.tokenString << "', the statement did not begin with an identifier or a keyword!!!");
		NextToken(lex);
		break;
	}


}

ASTExpression* Parser::ParseExpressionRHS(S32 exprPrec, ASTExpression* lhs) {
	while(true) {
		//If the token prec is less than 0 that means that this is not a binary opperator
		//And we dont have to do anything aside from returning the allready parsed expression
		auto tokenPrec = GetCurrentTokenPrecedence();
		if(tokenPrec < 0) {
			return lhs;
		}

		//We know that the currentToken is a binop
		auto binop = lex.token;
		auto binopStr = lex.tokenString;
		auto binopPos = lex.tokenSite;
		NextToken(lex);	//Eat the binop

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


// NOTE Parse Primary assumes the the lexer has already been incremented to the next token!
ASTExpression* Parser::ParsePrimaryExpression() {
	switch (lex.token) {

	// Handles variables, function calls!
	case Token::IDENTIFIER:
	{
		LOG_VERBOSE("Parsing an identifier expression! for identifier -> " << lex.tokenString);
		auto ident = FindIdentifierInScope(currentPackage, currentScope, lex.tokenString);
		if(!ident) {
			LOG_ERROR(lex.tokenSite << " identifier " << lex.tokenString << " does not exist!");
			NextToken(lex);
			return nullptr;
		}

		NextToken(lex); //Consume the identifier
		//@Duplicate of procedure inside of ParseStatement!
		if(lex.token == Token::ParenOpen) {
			LOG_VERBOSE("Parsing Call to: " << ident->name);
			ASTCall* call = CreateCall();
			call->ident = ident;

			NextToken(lex);
			while (lex.token != Token::ParenClose && lex.token != Token::UNKOWN && lex.token != Token::EndOfFile) {
				ASTExpression* expression = ParseExpression();
				if (expression == nullptr) {
					LOG_ERROR(lex.tokenSite << " Could not resolve expression at argument index" << call->args.size() << "in call to function " << ident->name);
					return nullptr;
				}
				call->args.push_back(expression);
			}
			NextToken(lex);	//Eat the close ')'
			return (ASTExpression*)call;
		}
		return (ASTExpression*)ident->node;
	}break;
	//This is a numeric literal!
	case Token::Number: {
		LOG_VERBOSE("Parsing a numberExpression!");
		auto dotPos = lex.tokenString.find(".");
		bool isFloat = dotPos == std::string::npos ? false : true;
		if (isFloat) {
			if(lex.tokenString.substr(dotPos + 1).find(".") != std::string::npos) {
				LOG_ERROR("Floating Point value contains two decimal points!");
			}
			auto value = std::stof(lex.tokenString);
			auto result = CreateFloatLiteral(value);
			NextToken(lex);	//Eats the float literal
			return result;
		} else {
			auto value = std::stoi(lex.tokenString);
			auto result = CreateIntegerLiteral(value);
			result->type = (ASTDefinition*) (FindIdentifierInScope(currentPackage, currentScope, "S32")->node);
			NextToken(lex); // Eats the integer literal
			return result;
		}
	}
		break;

	case Token::ScopeOpen:
		LOG_VERBOSE(lex.tokenSite << "Parsing a new scope :: BADDDDDD!!!!!!");
		return nullptr;

	case Token::EndOfFile:
		LOG_ERROR("HIT END OF FILE! THIS IS TERRIBLE YOU ARE MENTALY DISABLED, USER!");
		return nullptr;
	default:
		LOG_ERROR(lex.tokenSite << "Unknown token when expecting expression");
		NextToken(lex); // Increment the lexer to handle error recovery!
		return nullptr;
	}
	// This is dead code it will never happen.
	return nullptr;
}

ASTIdentifier* Parser::FindIdentifierInScope(Package* activePackage, ASTBlock* scope, std::string identString) {
		auto ident = scope->identifiers[identString];
		if(ident == nullptr) {
			if(scope->parent != nullptr)
				return FindIdentifierInScope(activePackage, scope->parent, identString);
			else {
				for(auto packageName : activePackage->importedPackages) {
					auto package = parsedPackages[packageName];
					ident = FindIdentifierInScope(package, &package->scope, identString);
					if(ident != nullptr)
						return ident;
				}
			}
		}

	return ident;
}

Package* Parser::CreatePackage(std::string filename) {
	auto package = parsedPackages[filename];
	assert(package == nullptr);
	package = new Package;
	package->importedPackages.push_back("primitives");
	parsedPackages[filename] = package;
	return package;
}

std::string TokenName(Token token) {
	switch (token) {
		case Token::IDENTIFIER: return "Identifier";
		case Token::ParenOpen: return "(";
		case Token::ParenClose: return ")";
		case Token::ScopeOpen: return "{";
		case Token::ScopeClose: return "}";
	}
}

bool Parser::ExpectAndEat(Token token) {
	if(lex.token != token) {
		LOG_ERROR("Lexer reported: " << TokenName(lex.token) << " expected: " << TokenName(token));
		NextToken(lex);
		return false;
	}
	NextToken(lex);
	return true;
}


void Parser::ParseFile(std::string filename) {
	auto oldPackage = currentPackage;
	currentPackage = CreatePackage(filename);
	currentScope = &currentPackage->scope;

	auto oldLexer = lexer;
	lexer = new Lexer(filename);
	NextToken(lex);	//Kicks of parsing... Grabs the first token in the file
	while (lex.token != Token::EndOfFile) {
		ParseStatement();
	}

	delete lexer;
	if(oldLexer != nullptr) {
		lexer = oldLexer;
	}

	if(oldPackage != nullptr) {
		currentPackage = oldPackage;
	}
}
