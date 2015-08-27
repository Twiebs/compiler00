#include "Parser.hpp"

/*
TODO a better way would be to store the precedences in a array
and directly lookup the value without branching... especialy if the
number of opperators begins to increase For now 'if' hax FTW!
*/

int GetTokenPrecedence(const Token& token);
ASTNode* ParseStatement(ParseState& parseState, Lexer& lex);
ASTExpression* ParseExpr(ParseState& parseState, Lexer& lex);
ASTNode* ParseReturn(ParseState& parseState, Lexer& lex);

ASTNode* ParseIdentifier(ParseState& parseState, Lexer& lex);
ASTNode* ParseIF(ParseState& parseState, Lexer& lex);
ASTNode* ParseIter(ParseState& state, Lexer& lex, const std::string& identName = "");
ASTNode* ParseBlock(ParseState& state, Lexer& lex, ASTBlock* block = nullptr);

// Determines the precedence level of the provided token
int GetTokenPrecedence(const Token& token) {
    if (token.type == TOKEN_ADD) return 20;
    if (token.type == TOKEN_SUB) return 20;
    if (token.type == TOKEN_MUL) return 40;
    if (token.type == TOKEN_DIV) return 40;
    return -1;
}

void ReportError(ParseState& parseState, FileSite& site, const std::string& msg) {
	parseState.errorCount++;
	std::cout << "[ERROR | " << site << "]" << msg;
}

void ParseFile (ParseState& parseState, Lexer& lex) {
	auto& token = lex.token;
	lex.next();
    while (token.type != TOKEN_EOF) {
        ParseStatement(parseState, lex);
    }
}

ASTNode* ParseStatement (ParseState& parseState, Lexer& lex) {
	LOG_INFO(lex.token.site << ": Parsing statement begining with: " << lex.token.string);
	switch(lex.token.type) {
	case TOKEN_IDENTIFIER: return ParseIdentifier(parseState, lex);
	case TOKEN_IF: 		return ParseIF(parseState, lex);
  case TOKEN_ITER:  return ParseIter(parseState, lex);
	case TOKEN_RETURN: 	return ParseReturn(parseState, lex);
  case TOKEN_SCOPE_OPEN: return ParseBlock(parseState, lex);
	default:
		LOG_ERROR("Could not parse statement for token: " << lex.token.string);
		lex.next();
		return nullptr;
	}
	return nullptr;
}

ASTNode* ParseReturn(ParseState& parseState, Lexer& lex) {
    LOG_VERBOSE(token.site << ": Parsing a return statement");
    lex.next();
    auto expr = ParseExpr(parseState, lex);
    auto returnVal = CreateReturnValue(expr);
    return returnVal;
}

ASTExpression* ParsePrimaryExpr(ParseState& parseState, Lexer& lex) {
	switch (lex.token.type) {
	case TOKEN_POINTER:
	case TOKEN_DEREF:
	case TOKEN_IDENTIFIER: {
		ExprAccess accessMode = EXPR_LOAD;
		if (lex.token.type == TOKEN_POINTER) {
			accessMode = EXPR_POINTER;
			lex.next();	//Eat the pointer token
		} else if (lex.token.type == TOKEN_DEREF) {
			accessMode = EXPR_DEREF;
			lex.next();	//Eat the deref token
		}

		LOG_VERBOSE("Parsing an identifier expression! for identifier: " << token.string);
		auto ident = FindIdentifier(parseState.currentScope, lex.token.string);
		if (!ident) {	// TODO defer identifier resolution
			ReportError(parseState, lex.token.site, "Identifier " + lex.token.string + " does not exist!");
			return nullptr;
		}

		lex.next(); // Eat the identifier
		if (lex.token.type == TOKEN_PAREN_OPEN) {
			LOG_VERBOSE("Parsing Call to: " << ident->name);
			ASTCall* call = CreateCall();
			call->ident = ident;

			lex.next();
			while (lex.token.type != TOKEN_PAREN_CLOSE && lex.token.type != TOKEN_UNKOWN && lex.token.type!= TOKEN_EOF) {
				ASTExpression* expression = ParseExpr(parseState, lex);
				if (expression == nullptr) {
					LOG_ERROR(lex.token.site << " Could not resolve expression at argument index" << call->args.size() << "in call to function " << ident->name);
					return nullptr;
				}
				call->args.push_back(expression);
			}
			lex.next();    //Eat the close ')'	//This all appears to be very bad
			// I probably should not have actualy done the lexer like this  But the parse and codegen
			// and stuff like this is deffinalty infinitly better.
			//We can just store the token inside the lexer and only copy out state if it a actualy required
			//This is much better there is no reason not to do thins because we are just copying around a token everywehere
			//Its much more convient and doesnt cost anything to just keep this state internal to the lexer like it orginialy was
			return (ASTExpression*)call;
		} else if (lex.token.type == TOKEN_ACCESS) {
			auto structVar = (ASTVariable*)ident->node;
			auto structDefn = (ASTStruct*)structVar->type;
			if(structDefn->nodeType != AST_STRUCT) ReportError(parseState, lex.token.site, "Identifier: " + ident->name + " does not name a struct type");


			auto expr = CreateMemberExpr(structVar);
			auto currentStruct = structDefn;
			while(lex.token.type == TOKEN_ACCESS) {
				lex.next(); // eat the member access
				auto memberIndex = GetMemberIndex(currentStruct, lex.token.string);
				if (memberIndex == -1) {
					ReportError(parseState, lex.token.site, "Identifier: " + lex.token.string + " does not name a member in struct");
				} else {
					expr->memberIndices.push_back(memberIndex);
					auto memberType = currentStruct->memberTypes[memberIndex];
					if(memberType->nodeType == AST_STRUCT)
						currentStruct = (ASTStruct*)memberType;
					else expr->type = memberType;
				}
				lex.next();	// eat the member ident
			}
			return expr;
		} else {
			auto var = (ASTVariable*)ident->node;
			auto expr = CreateVarExpr(var);
			expr->accessMode = accessMode;
			return expr;
		}

		LOG_ERROR("SOMTHING TERRIBLE HAS HAPPENED!");
	} break;

	    case TOKEN_NUMBER: {
	        LOG_VERBOSE("Parsing a numberExpression!");
	        auto dotPos = lex.token.string.find(".");
	        bool isFloat = dotPos == std::string::npos ? false : true;
	        if (isFloat) {
	            if(lex.token.string.substr(dotPos + 1).find(".") != std::string::npos) {
	                ReportError(parseState, lex.token.site, "Floating Point value contains two decimal points!");
	            }
	            auto value = std::stof(lex.token.string);
	            auto result = CreateFloatLiteral(value);
	            lex.next(); // Eat the float literal
	            return result;
	        } else {
	            auto value = std::stoi(lex.token.string);
	            auto result = CreateIntegerLiteral(value);
	            result->type = (ASTDefinition*) (FindIdentifier(parseState.currentScope, "S32")->node);
	            lex.next(); // Eat the int literal
	            return result;
	        }
	    }
	        break;

	    case TOKEN_SCOPE_OPEN:
	        LOG_VERBOSE(lex.tokenSite << "Parsing a new scope :: BADDDDDD!!!!!!");
	        return nullptr;

	    case TOKEN_EOF:
	        LOG_ERROR("HIT END OF FILE! THIS IS TERRIBLE YOU ARE MENTALY DISABLED, USER!");
	        return nullptr;
	    default:
	        LOG_ERROR(lex.token.site << "Unknown token when expecting expression");
	        //silllyyyyy // Increment the lexer to handle error recovery!
	        return nullptr;
	    }
	    // This is dead code it will never happen.
	    return nullptr;
}

ASTExpression* ParseExprRHS(int exprPrec, ASTExpression* lhs, ParseState& parseState, Lexer& lex) {
	while(true) {
				//If the token prec is less than 0 that means that this is not a binary opperator
				//And we dont have to do anything aside from returning the allready parsed expression
				auto tokenPrec = GetTokenPrecedence(lex.token);
				if(tokenPrec < 0) {
					return lhs;
				}

				//We know that the currentToken is a binop
				//Possibly?  Yes!
				auto binopToken = lex.token;
				lex.next();    //Eat the binop

				//We have a binop lets see what is on the other side!
				ASTExpression* rhs = ParsePrimaryExpr(parseState, lex);
				if(rhs == nullptr) {
					ReportError(parseState, binopToken.site, "Could not parse primary expression to the right of binary opperator '" + binopToken.string  + "'");
					return nullptr;
				}

				auto nextPrec = GetTokenPrecedence(lex.token);
				if(tokenPrec < nextPrec) {
					rhs = ParseExprRHS(tokenPrec + 1, rhs, parseState, lex);
					if(rhs == nullptr){
						LOG_ERROR("Could not parse recursive rhsParsing!");
						return nullptr;
					}
				}

				if(lhs->type != rhs->type) {
					LOG_ERROR("Type mismatch! Can not convert '" << lhs->type->identifier->name << "' to " << rhs->type->identifier->name << "' !");
					//TODO return null here?
				}

				// @Memory there is a leak here!
				// We dont know if this is just some temporary value or if it is actualy a true
				// Binary Opperation
				// This could be nested the most deep.
				// Like totaly uber deep. Just to be clear.
				lhs = (ASTExpression*)CreateBinaryOperation(binopToken.type, lhs, rhs);
			}   // Goes back to the while loop
}

ASTExpression* ParseExpr(ParseState& parseState, Lexer& lex) {
    auto lhs = ParsePrimaryExpr(parseState, lex);
    if(lhs == nullptr){
        return nullptr;
    }
    return ParseExprRHS(0, lhs, parseState, lex);
}

ASTNode* ParseIdentifier(ParseState& parseState, Lexer& lex) {
	auto identToken = lex.token;	//save the token for now
	auto ident = FindIdentifier(parseState.currentScope, lex.token.string);
	lex.next();	// Eat the identifier

	// The identifier that we are parsing may exist, may exit but be unresolved, or not exist at all
	// Depending on what we do with the identifer will determine if these factors matter
	switch(lex.token.type) {
	case TOKEN_TYPE_DECLARE: {
		if (ident != nullptr) {
			ReportError(parseState, identToken.site, " redefintion of identifier " + identToken.string + " first declared at site TODO SITE");
			parseState.flags |= PACKAGE_INVALID;
		}

		lex.next(); // Eat the ':'


    if (lex.token.type == TOKEN_ITER) {
      auto iter = ParseIter(parseState, lex, identToken.string);
      return iter;
    }

    else {
    	ident = CreateIdentifier(parseState.currentScope, identToken.string);
  		auto var = CreateVariable(parseState.currentScope);
  		var->identifier = ident;	// This is terrible
  		ident->node = var;

    	if (lex.token.type == TOKEN_POINTER) {
			var->isPointer = true;
    		lex.next(); //Eat the pointer token
    	}

  		ASTIdentifier* typeIdent = nullptr;
  		if (lex.token.type != TOKEN_IDENTIFIER) {
  			ReportError(parseState, lex.token.site, "type token '" + lex.token.string + "' is not an idnetifier!");
  		} else {
  			typeIdent = FindIdentifier(parseState.currentScope, lex.token.string);
  			if (!typeIdent) {
  				ReportError(parseState, lex.token.site, "could not resolve typde identifier '" + lex.token.string + "'!");
  			}
  		}

  		var->type = (ASTDefinition*) typeIdent->node;

  		// Now we determine if this variable will be initalzied
  		lex.next(); //eat type
  		if (lex.token.type == TOKEN_EQUALS) {
  			lex.next();    //Eat the assignment operator
  			var->initalExpression = ParseExpr(parseState, lex);	// We parse the inital expression for the variable!
  			LOG_INFO(ident->site << "Identifier(" << ident->name << ") of Type(" << typeIdent->name << ") declared with an inital expression specified!");
  		} else {
  			LOG_INFO(ident->site << "Identifier(" << ident->name << ") of Type(" << typeIdent->name << ") declared!");
  		}
  		return var;
    }
	} break;


	case TOKEN_TYPE_INFER: {
		LOG_ERROR("Unsupported type inference feature!");
		return nullptr;
	} break;


	case TOKEN_TYPE_DEFINE: {
		LOG_VERBOSE("Parsing TypeDefine");
		lex.next(); //Eat the typedef
		if (lex.token.type == TOKEN_PAREN_OPEN) {
			LOG_VERBOSE("Parsing FunctionDefinition");

			ASTFunctionSet* funcSet;
			if (ident == nullptr) {	// The identifier is null so the function set for this ident has not been created
				ident = CreateIdentifier (parseState.currentScope, identToken);
				funcSet = CreateFunctionSet (ident, parseState.currentScope);
			} else {
				assert(ident->node->nodeType == AST_FUNCTION);
				funcSet = (ASTFunctionSet*)ident->node;
			}

			auto function = CreateFunction (funcSet);
			function->ident = ident;
			parseState.currentScope = function;
			lex.next(); // Eat the open paren

			while (lex.token.type != TOKEN_PAREN_CLOSE) {
				ASTNode* node = ParseStatement(parseState, lex);
				if (node == nullptr) {
					ReportError(parseState, lex.token.site, "Could not parse arguments for function definition " + identToken.string);
				} else if (node->nodeType != AST_VARIABLE) {
					ReportError(parseState, lex.token.site, "Function argument is not a varaibe!");
				} else {
					auto var = (ASTVariable*) node;
					function->args.push_back(var);
				}
			}
			lex.next(); // Eat the close ')'

			if (lex.token.type == TOKEN_TYPE_RETURN) {
				lex.next();
				if (lex.token.type != TOKEN_IDENTIFIER) {
					LOG_ERROR("expected a type after the return operator");
					return nullptr;
				}

				auto returnTypeIdent = FindIdentifier(parseState.currentScope, lex.token.string);
				if (returnTypeIdent == nullptr) {
					LOG_ERROR(lex.token.site << ": Could not resolve return type for function " << identToken.string);
					return nullptr;
				} else if (returnTypeIdent->node->nodeType != AST_DEFINITION && returnTypeIdent->node->nodeType != AST_STRUCT) {
					ReportError(parseState, lex.token.site, "Identifier " + lex.token.string + " does not name a type");
					// No nullptrs returned when parsing ... we need to continue to increment the lexer and
					// do better error recovery
				}

				function->returnType = (ASTDefinition*)returnTypeIdent->node;
				lex.next();
			}

			// There was no type return ':>' operator after the argument list but an expected token followed.
			// We assume it was intentional and that the return type is implicitly void
			else if (lex.token.type == TOKEN_SCOPE_OPEN || lex.token.type == TOKEN_FOREIGN) {
				function->returnType = global_voidType;
			} else {
				LOG_ERROR(lex.token.site << "A new scope or foreign keyword must follow a function definition");
			}

		auto func = FindMatchingFunction(ident, function);
		if (func != nullptr) {
			ReportError(parseState, identToken.site, "Function re-definition!  Overloaded function " + identToken.string + "was already defined!");
		} else if (lex.token.type == TOKEN_SCOPE_OPEN) {
			// TODO change this to parseStatement to get the nextblock
			// A new scope has been opened...
			lex.next(); // Eat the scope

			while (lex.token.type != TOKEN_SCOPE_CLOSE && lex.token.type != TOKEN_EOF) {
				ASTNode* node = ParseStatement(parseState, lex);
				if (node == nullptr) {
					LOG_ERROR(lex.token.site << " Could not parse statement inside function body: " << ident->name);
					return nullptr;
				}
				function->members.push_back(node);
			}

		} else if (lex.token.type != TOKEN_FOREIGN) {
			LOG_ERROR("Expected a new scope to open after function definition!");
			LOG_INFO("Did you misspell foreign?");
			return nullptr;
		} else {
			if(function->parent->parent != nullptr) {
				// TODO why wouldn't we allow this?
				LOG_ERROR("Cannot create a foreign function nested in another block!  Foreign functions must be declared in the global scope!");
				return nullptr;
			}
		}

		lex.next();    // Eats the foreign or the end of the scope
		parseState.currentScope = function->parent;
		return function;
	}

  // NOTE @STRUCT
  else if (lex.token.type == TOKEN_STRUCT) {
    LOG_VERBOSE("Parsing a struct definition");
    LOG_INFO("Parsing a struct definition!");
    lex.next(); // Eat the struct keyword

    if (ident != nullptr) {
    	ReportError(parseState, identToken.site, "Struct Redefinition");
    }

    ident = CreateIdentifier(parseState.currentScope, identToken);

    if (lex.token.type == TOKEN_SCOPE_OPEN) {
      lex.next(); // Eat the open scope

      auto structDefn = CreateStruct();
      ident->node = structDefn;

      while(lex.token.type != TOKEN_SCOPE_CLOSE) {
        if (lex.token.type != TOKEN_IDENTIFIER) {
          ReportError(parseState, lex.token.site, "All top-level statements inside struct defns must be identifiers");
        }

        auto memberToken = lex.token; // Copy the ident token
        lex.next(); // Eat the identifier token;

        if (lex.token.type != TOKEN_TYPE_DECLARE) {
          ReportError(parseState, lex.token.site, "All identifiers inside a struct defn must be used with a type declare");
        }
        lex.next(); // Eat the typedecl
        auto typeIdent = FindIdentifier(parseState.currentScope, lex.token.string);
        if (!typeIdent) {
          ReportError(parseState, lex.token.site, "Could not reslove type " + lex.token.string);
        }

        auto typedefn = (ASTDefinition*)typeIdent->node;
        structDefn->memberNames.push_back(memberToken.string);
        structDefn->memberTypes.push_back(typedefn);
        lex.next(); //eat the type ident
      }

      assert(structDefn->memberNames.size() == structDefn->memberTypes.size());
		lex.next(); //Eat the close scope

		if (structDefn->memberTypes.size() > 0) {
			std::vector<llvm::Type*> memberTypes;
			for(auto type : structDefn->memberTypes)
				memberTypes.push_back(type->llvmType);
			structDefn->llvmType = llvm::StructType::create(memberTypes, "Vector3");
		} else {
			ReportError(parseState, identToken.site, "Structs must contain at least one member");
		}
    } else {
    	ReportError(parseState, lex.token.site, "Structs must be defined with a block");
    }

    return ParseStatement(parseState, lex); // Consider the struct handeled and just get another node
    //Most of this requiring to return a node on parsing is a reminatnt of the epxression pased
    // functional stype language where everything is considered an expression
  }
} break;

	case TOKEN_PAREN_OPEN:  {
		LOG_VERBOSE("Attempting to parse call to : " << identToken.string);
		if (ident == nullptr) {
			LOG_ERROR("function named " << identToken.string << " does not exist");
			return nullptr;
		}

		//Create the call and now determine its arguments
		ASTCall* call = CreateCall();
		call->function = nullptr;
		call->ident = ident;

		lex.next(); //Eat the open Paren
		while (lex.token.type != TOKEN_PAREN_CLOSE && lex.token.type != TOKEN_UNKOWN && lex.token.type != TOKEN_EOF) {
			ASTExpression* expr = ParseExpr(parseState, lex);
			if(expr == nullptr) {
				LOG_ERROR(lex.token.site << " Could not resolve expression for argument at index " << call->args.size() << " in call to function " << ident->name);
				//DONT return here... just keep going so we can find more errors'
				continue;    //But we do skip pushing the expression on to the function arguments
				//it might be better to keep the nullptr so that we can determine the actually amount of arrugments that were specified by the user for better error reporting!
			}
			call->args.push_back(expr);
		}    //We push back all the arguments and don't care about what function we are actually going to end up calling...
			//We may or may not actually find the function that we are looking for!
		lex.next();    //Eat the close ')'

		auto funcSet = (ASTFunctionSet*)ident->node;
		if(funcSet->functions.size() == 0) {
			LOG_ERROR("There are no functions named " << identToken.string);
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
	} break;

	case TOKEN_ACCESS: {
		if (ident == nullptr) {
			ReportError(parseState, identToken.site, "Could not resolve identifier " + identToken.string + " when trying to acess member");
		}

		// TODO this could also be a namespace / enum / whatever
		auto structVar = (ASTVariable*)ident->node;
		auto structDefn = (ASTStruct*)structVar->type;
		if(structDefn->nodeType != AST_STRUCT) ReportError(parseState, identToken.site, "Member access operator only applies to struct types!");

		auto memberAccess = CreateMemberAccess(structVar);
		auto currentStruct = structDefn;

		while (lex.token.type == TOKEN_ACCESS) {
			lex.next();	// Eat the access token
			if (lex.token.type != TOKEN_IDENTIFIER) ReportError(parseState, lex.token.site, "Member access must reference an identifier");
			auto memberIndex = GetMemberIndex(currentStruct, lex.token.string);
			if (memberIndex == -1) ReportError(parseState, lex.token.site, "Struct " + ident->name + "does not contain any member named " + lex.token.string);
			memberAccess->memberIndices.push_back(memberIndex);

			auto memberType = currentStruct->memberTypes[memberIndex];
			if(memberType->nodeType == AST_STRUCT) currentStruct = (ASTStruct*)memberType;
			lex.next();	// Eat the member identifier
		}

		switch (lex.token.type) {
		case TOKEN_EQUALS:
			lex.next();
			memberAccess->mode = ACCESS_ASSIGN;
			memberAccess->expr = ParseExpr(parseState, lex);
			break;
		case TOKEN_ADD_EQUALS:
			lex.next();
			memberAccess ->mode = ACCESS_ADD;
			memberAccess ->expr = ParseExpr(parseState, lex);
			break;
		case TOKEN_SUB_EQUALS:
			lex.next();
			memberAccess ->mode = ACCESS_SUB;
			memberAccess ->expr = ParseExpr(parseState, lex);
			break;
		case TOKEN_MUL_EQUALS:
			lex.next();
			memberAccess ->mode = ACCESS_MUL;
			memberAccess ->expr = ParseExpr(parseState, lex);
			break;
		case TOKEN_DIV_EQUALS:
			lex.next();
			memberAccess ->mode = ACCESS_DIV;
			memberAccess ->expr = ParseExpr(parseState, lex);
			break;
		default:
			// Dont eat the op because this is probably the end of the statement.
			// or this is being parsed in an expression
			// And this code is probably dead
			LOG_ERROR("Revised oppion.  This is allmost certianly dead code");
			LOG_ERROR("This code is (probably) dead!  Come and check it out if you see this");
			memberAccess ->mode = ACCESS_LOAD;
			memberAccess ->expr = nullptr;
		}
		return memberAccess;

	} break;

	case TOKEN_EQUALS:
	case TOKEN_ADD_EQUALS:
	case TOKEN_SUB_EQUALS:
	case TOKEN_MUL_EQUALS:
	case TOKEN_DIV_EQUALS:
	case TOKEN_MOD_EQUALS:
		if(ident == nullptr) {
			LOG_ERROR(lex.token.site << "cannot parse variable mutation on unkown identifier" << identToken.string);
			parseState.flags |= PACKAGE_INVALID;
		}

        lex.next();    //Eat mutation
		auto var = (ASTVariable*)ident->node;
		if(var->nodeType != AST_VARIABLE) {
			LOG_ERROR(identToken.site << ": Recognized identifier " << identToken.string << " but it is not a variable!  Its a " << ToString(var->nodeType));
		}

		// TODO
		//This is actualy a good spot to return null because these are top level errors!
		//Make a new error called top level error that specifies actual errors in the program
		//Secondary errors might not be actual errors once you fix primary errors
		auto expr = ParseExpr(parseState, lex);
		if (expr == nullptr) {
			//For now binops will auto resolve themselves based on the respective rhs and lhs expressions inthe
			//operation
			LOG_ERROR("Could not parse expression on the right of the assignment operator");

			// Which is actualy totaly cool!
			// We dont actualy care if this doesnt have a resolved type... because there is some tom foolay invloved
			//With headerless compliation.. However we can simplfy this probelm for now by simply ingoring it!
			// YEs! thats totaly the answer to everything in life.. Just Ingnore it.
			// ha no.
			return nullptr;
		} else if (expr->type != var->type) {
			if (expr->type == nullptr) {
				LOG_ERROR("Internal Compiler error: Expression is a nullptr");
				return nullptr;
			}
			LOG_ERROR(identToken.site << "Type mismatch between variable " << identToken.string << "(" << var->type->identifier->name << ") and RHS expression(" << expr->type->identifier->name << ")!");
			return nullptr;
		}
		//Why do variables need anyt ype of mutation whatsofever?
		//That doesnt even make any sense whatso ever
		return CreateMutation(var, expr);
	}

	//We have gotten past all our routines
	//THIS SHOULD NEVER HAPPEN!
	INTERNAL_ERROR(lex.token.site << "Something REALLY Terrible has happened!  ITS IMPOSSIBLE TO GET HERE! The lex.token that caused this was [" << lex.token.string << "]");
	return nullptr;
}

ASTNode* ParseIF(ParseState& parseState, Lexer& lex) {
    LOG_VERBOSE(lex.token.site << " Parsing an if statement!");
    lex.next();	//Eat the IF token

	auto expr = ParseExpr(parseState, lex);
	if (!expr) {
		LOG_ERROR("Could not evaluate expression when parsing if statement!");
		return nullptr;
	}

    if(lex.token.type == TOKEN_SCOPE_OPEN) {
        lex.next(); //Eat the open scope
        auto ifStatement = CreateIfStatement(expr);
        ifStatement->ifBlock = CreateBlock(parseState.currentScope);

        auto previousScope = parseState.currentScope;
        parseState.currentScope = ifStatement->ifBlock;
		while (lex.token.type != TOKEN_SCOPE_CLOSE) {
            auto node = ParseStatement(parseState, lex);
            if (!node) {
                LOG_ERROR("Could not parse statement inside of IF statement");
            } else {
                parseState.currentScope->members.push_back(node);
            }
        }

        parseState.currentScope = previousScope;
        lex.next(); //Eat the '}'
        if (lex.token.type == TOKEN_ELSE) {
            LOG_VERBOSE("Parsing else statement");
            lex.next(); //Eat the else keyword!
            if (lex.token.type == TOKEN_SCOPE_OPEN) {
                lex.next();//Eat the '{'
                ifStatement->elseBlock = CreateBlock(parseState.currentScope);
                previousScope = parseState.currentScope;
                parseState.currentScope = ifStatement->elseBlock;
                while(lex.token.type != TOKEN_SCOPE_CLOSE) {
                    auto node = ParseStatement(parseState, lex);
                    if (!node) {
                        LOG_ERROR("Could not parse statement inside of IF statement");
                    } else {
                        parseState.currentScope->members.push_back(node);
                    }
                }
                parseState.currentScope = previousScope;
                lex.next(); //Eat the '}'
            } else if (lex.token.type == TOKEN_IF){
            	//Interestingly enough this actualy is slightly cleaner
                ifStatement->elseBlock = (ASTBlock*)ParseStatement(parseState, lex);
            } else {
                LOG_ERROR("Expected a new scope to open after else keyword or the if keyword!! for else if statements dawggggggggggg!!!");
            }
        }
        return ifStatement;
    } else {
        LOG_ERROR("expected an open scope after if expression!");
        return nullptr;
    }
}

ASTNode* ParseIter(ParseState& state, Lexer& lex, const std::string& identName) {
  LOG_VERBOSE(lex.token.site << "Parsing a iter statement");
  lex.next(); // Eat the iter

  auto expr = ParseExpr(state, lex);
  if (!expr) LOG_ERROR(lex.token.site << "Could not parse expression to the right of iter");

  if (lex.token.type == TOKEN_TO) {
	lex.next(); 	//Eat the to
    if (identName != "") {
      auto block = CreateBlock(state.currentScope);
      auto ident = CreateIdentifier(block, identName);
      auto var = CreateVariable(block);
      var->type = global_S32Type;	//HACK
      var->identifier = ident;
      var->initalExpression = expr;
      ident->node = var;

      auto endExpr = ParseExpr(state, lex);
      if (!endExpr) LOG_ERROR(lex.token.site << "Could not parse Expression after TO keyword");

      ParseBlock(state, lex, block);
      auto iter = CreateIter(ident, expr, endExpr, nullptr, block);

      return iter;

    } else {
      LOG_ERROR(lex.token.site << "iter statement must be declared with an identifier!");
    }
  }

  else if (lex.token.type == TOKEN_SCOPE_OPEN) {
    auto block = ParseBlock(state, lex);
    LOG_ERROR("Not dealing with these for now");
  }
  return nullptr;
}

ASTNode* ParseBlock(ParseState& state, Lexer& lex, ASTBlock* block) {
  LOG_VERBOSE("Parsing a new block");

  lex.next();   // Eat the SCOPE_OPEN
  auto previousScope = state.currentScope;

  if (block == nullptr) {
    block = CreateBlock(previousScope);
  }
  state.currentScope = block;

  while(lex.token.type != TOKEN_SCOPE_CLOSE && lex.token.type != TOKEN_EOF) {
    auto node = ParseStatement (state, lex);
    if (node == nullptr) {
      LOG_ERROR(lex.token.site << "Could not parse statement inside block");
    } else {
      block->members.push_back(node);
    }
  }

  lex.next();  //Eat the close scope
  state.currentScope = previousScope;
  return block;
}
