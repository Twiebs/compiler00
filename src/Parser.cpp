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
	std::cout << "ERROR" << site << " " << msg  << "\n";
}

ASTNode* ParseImport(ParseState& state, Lexer& lex) {
	lex.next(); //Eat the import statement
	if (lex.token.type != TOKEN_STRING)
		ReportError(state, lex.token.site, "Import keyword requires a string to follow it");
	else
		state.importedFiles.push_back(lex.token.string);
	lex.next();	 //Eat the import string
	return ParseStatement(state, lex);
}

//TODO seperate ASTNode into two differently treated branches of the AST
// A statement either begins with an identifier, a keyword, or a new block

ASTNode* ParseStatement (ParseState& state, Lexer& lex) {
	switch (lex.token.type) {
	case TOKEN_IDENTIFIER:  return ParseIdentifier(state, lex);
	case TOKEN_IF: 		    return ParseIF(state, lex);
  	case TOKEN_ITER:        return ParseIter(state, lex);
	case TOKEN_RETURN: 	    return ParseReturn(state, lex);
	case TOKEN_SCOPE_OPEN:  return ParseBlock(state, lex);
	case TOKEN_IMPORT:		return ParseImport(state, lex);
	default:
		ReportError(state, lex.token.site, "Could not parse statement: unkown Token");
		lex.next(true);
		return nullptr;
	}
}

ASTNode* ParseReturn(ParseState& state, Lexer& lex) {
    LOG_VERBOSE(lex.token.site << ": Parsing a return statement");
    lex.next();
    auto expr = ParseExpr(state, lex);
    auto returnVal = CreateReturnValue(&state.arena, expr);
    return returnVal;
}

ASTCall* ParseCall(ParseState& state, Lexer& lex, const Token& identToken) {
  std::vector<ASTExpression*> args;
  lex.next(); // Eat the open paren
  while (lex.token.type != TOKEN_PAREN_CLOSE) {
    ASTExpression* expr = ParseExpr(state, lex);
    if (expr == nullptr) {
      ReportError(state, lex.token.site, " Could not resolve expression for argument at index:XXX in call to function named" + identToken.string);
    } else {
      args.push_back(expr);
    }
  } lex.next();    // Eat the close paren

  ASTCall* call = CreateCall(&state.arena, &args[0], args.size());
  auto ident = FindIdentifier(state.currentScope, identToken.string);
  if (ident == nullptr) {
    AddDependency(identToken.string, call);
} else {
  auto funcSet = (ASTFunctionSet*)ident->node;
  assert(funcSet->nodeType == AST_FUNCTION);
  call->function = FindFunction(funcSet, (ASTExpression**)&args[0], args.size());
  if (!call->function) AddDependency(identToken.string, call);
}
  return call;
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

		LOG_VERBOSE("Parsing an identifier expression! for identifier: " << lex.token.string);
		auto ident = FindIdentifier(parseState.currentScope, lex.token.string);
		if (!ident) {
			ReportError(parseState, lex.token.site, "Identifier " + lex.token.string + " does not exist!");
      lex.next();
			return nullptr;
		}

    Token identToken = lex.token;
		lex.next(); // Eat the identifier
		if (lex.token.type == TOKEN_PAREN_OPEN) {
      auto call = ParseCall(parseState, lex, identToken);
			return (ASTExpression*)call;
		} else if (lex.token.type == TOKEN_ACCESS) {
			auto structVar = (ASTVariable*)ident->node;
			auto structDefn = (ASTStruct*)structVar->type;
			if(structDefn->nodeType != AST_STRUCT) ReportError(parseState, lex.token.site, "Identifier: " + ident->name + " does not name a struct type");


			auto currentStruct = structDefn;
			std::vector<U32> indices;
			ASTDefinition* exprType;
			while(lex.token.type == TOKEN_ACCESS) {
				lex.next(); // eat the member access
				auto memberIndex = GetMemberIndex(currentStruct, lex.token.string);
				if (memberIndex == -1) {
					ReportError(parseState, lex.token.site, "Identifier: " + lex.token.string + " does not name a member in struct");
				} else {
					indices.push_back(memberIndex);
					auto memberType = currentStruct->memberTypes[memberIndex];
					if(memberType->nodeType == AST_STRUCT)
						currentStruct = (ASTStruct*)memberType;
					else exprType = memberType;
				}
				lex.next();	// eat the member ident
			}
			auto expr = CreateMemberExpr(&parseState.arena, structVar, &indices[0], indices.size());
			expr->type = exprType;
			return expr;
		} else {
			auto var = (ASTVariable*)ident->node;
			auto expr = CreateVarExpr(&parseState.arena, var);
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
            auto result = CreateFloatLiteral(&parseState.arena, value);
            lex.next(); // Eat the float literal
            return result;
        } else {
            auto value = std::stoi(lex.token.string);
            auto result = CreateIntegerLiteral(&parseState.arena, value);
            result->type = (ASTDefinition*) (FindIdentifier(parseState.currentScope, "S32")->node);
            lex.next(); // Eat the int literal
            return result;
        }
    } break;

    case TOKEN_STRING: {
      LOG_VERBOSE("Parsing a string expression...");
      LOG_VERBOSE("WOOF WOOF WOOF! WORK SILLY DEBUGER WORKK!!! STOP HAVING BUGGSS ITS YOUR JOB TO BE THE OPPOSITEOF THAT!!!!!!");
      auto str = CreateStringLiteral (&parseState.arena, lex.token.string);
      lex.next(); // Eat the string token
      return str;
    } break;

    default:
        LOG_ERROR(lex.token.site << "Unknown token when expecting expression");
        //silllyyyyy // Increment the lexer to handle error recovery!
        return nullptr;
    }
    // This is dead code it will never happen.
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
				lhs = (ASTExpression*)CreateBinaryOperation(&parseState.arena, binopToken.type, lhs, rhs);
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
	auto state = parseState;
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
  		auto var = CreateVariable(&parseState.arena, parseState.currentScope);
  		var->identifier = ident;	// This is terrible
  		ident->node = var;

    	if (lex.token.type == TOKEN_POINTER) {
			var->isPointer = true;
    		lex.next(); // Eat the pointer token
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
  		lex.next(); // eat type
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
		lex.next(); // Eat the typedef
		if (lex.token.type == TOKEN_PAREN_OPEN) {
			LOG_VERBOSE("Parsing FunctionDefinition");

			ASTFunctionSet* funcSet;
			if (ident == nullptr) {	// The identifier is null so the function set for this ident has not been created
				if (identToken.string == "Main") identToken.string = "main";
				ident = CreateIdentifier (parseState.currentScope, identToken);
				funcSet = CreateFunctionSet (ident, parseState.currentScope);
			} else {
				assert(ident->node->nodeType == AST_FUNCTION);
				funcSet = (ASTFunctionSet*)ident->node;
			}

			auto function = CreateFunction(funcSet);
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
		if (func != nullptr && func != function) {
			ReportError(parseState, identToken.site, "Function re-definition!  Overloaded function " + identToken.string + "was already defined!");
		} else if (lex.token.type == TOKEN_SCOPE_OPEN) {
			// TODO change this to parseStatement to get the nextblock
			// A new scope has been opened...
			lex.next(); // Eat the scope

			while (lex.token.type != TOKEN_SCOPE_CLOSE && lex.token.type != TOKEN_EOF) {
        // Here we are going to push back nullptrs into the function because they will never
        // Get to the codegenration phase anyway..  Instead of branching we can juse do this and not care
				ASTNode* node = ParseStatement(parseState, lex);
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
      structDefn->identifier = ident;
      ident->node = structDefn;

      while(lex.token.type != TOKEN_SCOPE_CLOSE) {
        if (lex.token.type != TOKEN_IDENTIFIER) {
          ReportError(parseState, lex.token.site, "All statements inside structDefns must be identifier decls");
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
        parseState.currentScope->members.push_back(structDefn);
  		} else {
  			ReportError(parseState, identToken.site, "Structs must contain at least one member");
  		}
    } else {
    	ReportError(parseState, lex.token.site, "Structs must be defined with a block");
    }

    return ParseStatement(parseState, lex); // Consider the struct handeled and just get another node
    //Most of this requiring to return a node on parsing is a reminatnt of the epxression pased
    // functional stype language where everything is considered an expression
  } else {
	  ReportError(parseState, lex.token.site, "Could not define type with identifier: " + identToken.string +" (unknown keyword '" + lex.token.string + "')");
	  lex.next();
	  return nullptr;
  }
} break;
  case TOKEN_PAREN_OPEN:  {
		LOG_VERBOSE("Attempting to parse call to : " << identToken.string);
    auto call = ParseCall(parseState, lex, identToken);
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

		auto currentStruct = structDefn;
    std::vector<U32> memberIndices;
		while (lex.token.type == TOKEN_ACCESS) {
			lex.next();	// Eat the access token
			if (lex.token.type != TOKEN_IDENTIFIER) ReportError(parseState, lex.token.site, "Member access must reference an identifier");
			auto memberIndex = GetMemberIndex(currentStruct, lex.token.string);
			if (memberIndex == -1) ReportError(parseState, lex.token.site, "Struct " + ident->name + "does not contain any member named " + lex.token.string);
			memberIndices.push_back(memberIndex);

			auto memberType = currentStruct->memberTypes[memberIndex];
			if(memberType->nodeType == AST_STRUCT) currentStruct = (ASTStruct*)memberType;
			lex.next();	// Eat the member identifier
		}

    Operation operation;
		switch (lex.token.type) {
		case TOKEN_EQUALS:      operation = OPERATION_ASSIGN; break;
		case TOKEN_ADD_EQUALS:  operation = OPERATION_ADD;    break;
		case TOKEN_SUB_EQUALS:  operation = OPERATION_SUB;    break;
		case TOKEN_MUL_EQUALS:  operation = OPERATION_MUL;    break;
		case TOKEN_DIV_EQUALS:  operation = OPERATION_DIV;    break;
    default: ReportError(parseState, lex.token.site, "Unkown operator: " + lex.token.string);
    } lex.next();

    auto expr = ParseExpr(parseState, lex);
    auto memberOperation = CreateMemberOperation(&parseState.arena, structVar, operation, expr, &memberIndices[0], memberIndices.size());
    return memberOperation;

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
		// Why do variables need anyt ype of mutation whatsofever?
		// That doesnt even make any sense whatso ever
		return CreateVariableOperation(&parseState.arena, var, expr);
	}

	// We have gotten past all our routines
	// THIS SHOULD NEVER HAPPEN!
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
  if (!expr)
    LOG_ERROR(lex.token.site << "Could not parse expression to the right of iter");

  if (lex.token.type == TOKEN_TO) {
	   lex.next(); 	//Eat the to
     if (identName != "") {
      auto block = CreateBlock(state.currentScope);
      auto ident = CreateIdentifier(block, identName);
      auto var = CreateVariable(&state.arena, block);
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
      ReportError(state, lex.token.site, "iter statements that iterrate through a range must be declared with an identifier to hold the index!");
      ParseExpr(state, lex);  //eat the other expr
      //TODO skip block or somthing?
      return nullptr;
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

void ParseFile(ParseState& state, const std::string& rootDir, const std::string& filename) {
	Lexer lex;
	lex.stream.open(rootDir + filename);
	if (!lex.stream.is_open()) {
		LOG_ERROR("Could not open file " + filename);
		return;
	}
	lex.nextChar = lex.stream.get();
  lex.token.site.filename = filename;

	auto& token = lex.token;
	lex.next();
	while (token.type != TOKEN_EOF) {
		ParseStatement(state, lex);
	}
}
