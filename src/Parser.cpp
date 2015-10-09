#include <string.h>
#include <stdarg.h>

#include "Common.hpp"
#include "AST.hpp"
#include "Build.hpp"


void PushWork (const std::string& filename);

void EatLine(Worker* worker);
void NextToken(Worker* worker);
internal inline int GetTokenPrecedence(const Token& token);

internal ASTNode* ParseStatement(Worker* worker);
internal ASTExpression* ParseExpr(Worker* worker);
internal ASTNode* ParseReturn(Worker* worker);

internal inline ASTNode* ParseIdentifier(Worker* worker);
internal inline ASTNode* ParseIF(Worker* worker);
internal inline ASTNode* ParseIter(Worker* worker, const std::string& identName = "");
internal inline ASTNode* ParseBlock(Worker* worker, ASTBlock* block = nullptr);

void ReportError (Worker* worker, FileSite* site, const char* msg, ...);
void ReportError (Worker* worker, FileSite& site, const std::string& msg);
void ReportError (Worker* worker, const std::string& msg);

internal inline int GetTokenPrecedence (const Token& token) {
    if (token.type == TOKEN_LOGIC_OR) return 5;
    if (token.type == TOKEN_LOGIC_AND) return 5;
	if (token.type == TOKEN_LOGIC_LESS)           return 10;
    if (token.type == TOKEN_LOGIC_GREATER)        return 10;
    if (token.type == TOKEN_LOGIC_LESS_EQUAL)     return 10;
    if (token.type == TOKEN_LOGIC_GREATER_EQAUL)  return 10;
    if (token.type == TOKEN_ADD) return 20;
	if (token.type == TOKEN_SUB) return 20;
	if (token.type == TOKEN_MUL) return 40;
	if (token.type == TOKEN_DIV) return 40;
	return -1;
}

void ReportError(Worker* worker, FileSite& site, const std::string& msg) {
	worker->errorCount++;
	std::cout << site << " \x1b[31m" << msg	<< "\033[39m\n";
}

void ReportError(Worker* worker, const std::string& msg) {
    worker->errorCount++;
    std::cout << " \x1b[31m" << msg	<< "\033[39m\n";
}

void ReportError(Worker* worker, const char* msg, ...) {
    worker->errorCount++;
    va_list args;
    printf(" \x1b[31m");
    va_start(args, msg);
    vprintf(msg, args);
    va_end(args);
    printf("\033[39m\n");
}

void ReportError(Worker* worker, FileSite* site, const char* msg, ...) {
    worker->errorCount++;
    va_list args;
    std::cout << *site;
    printf(" \x1b[31m");
    va_start(args, msg);
    vprintf(msg, args);
    va_end(args);
    printf("\033[39m\n");
}

ASTNode* ParseImport(Worker* worker) {
	NextToken(worker); // Eat the import statement
	if (worker->token.type != TOKEN_STRING) {
		ReportError(worker, worker->token.site, "Import keyword requires a string to follow it");
		EatLine(worker);	//TODO this is a minor hack ...
	}
	else {
		PushWork(worker->token.string);
	}
	NextToken(worker);	 // Eat the import string
	return ParseStatement(worker);
}

ASTNode* ParseStatement (Worker* worker) {
	switch (worker->token.type) {
	case TOKEN_ADD:
	case TOKEN_SUB:
	case TOKEN_MUL:
	case TOKEN_DIV:

	case TOKEN_IDENTIFIER:	      	return ParseIdentifier(worker);
	case TOKEN_IF: 					return ParseIF(worker);
	case TOKEN_ITER:				return ParseIter(worker);
	case TOKEN_RETURN: 			  	return ParseReturn(worker);
	case TOKEN_BLOCK_OPEN:	      	return ParseBlock(worker);
	case TOKEN_IMPORT:			  	return ParseImport(worker);
	default:
		ReportError(worker, &worker->token.site, "Could not parse statement unkown token(%s)", worker->token.string.c_str());
		NextToken(worker);
		return nullptr;
	}
}

internal void SkipEntireBlock (Worker* worker) {
    int blockLevel = 1;
    while (blockLevel > 0) {
        if (worker->token.type == TOKEN_BLOCK_OPEN)
            blockLevel++;
        else if (worker->token.type == TOKEN_BLOCK_CLOSE)
            blockLevel--;
        NextToken(worker);
    }
}

ASTNode* ParseReturn(Worker* worker) {
		LOG_VERBOSE(worker->token.site << ": Parsing a return statement");
		NextToken(worker);
		auto expr = ParseExpr(worker);
		auto returnVal = CreateReturnValue(&worker->arena, expr);
		return returnVal;
}

internal inline ASTCast* ParseCast(Worker* worker, const Token& identToken) {
    NextToken(worker);
    auto typeDefn = (ASTDefinition*)FindNodeWithIdent(worker->currentBlock, identToken.string);
    assert(typeDefn->nodeType == AST_DEFINITION);
    auto expr = ParseExpr(worker);

    if (worker->token.type != TOKEN_PAREN_CLOSE) {
        ReportError(worker, &worker->token.site, "expected close parren enlosing expression when casting to type %s", identToken.string.c_str());
        return nullptr;
    } else {
        NextToken(worker);
        auto cast = CreateCast(&worker->arena, typeDefn, expr);
        return cast;
    }
}

ASTCall* ParseCall(Worker* worker, const Token& identToken) {
    if (worker->currentBlock->parent == nullptr) {
        ReportError(worker, worker->token.site, "Can not call functions outside a block!  Did you mean to use :: ?");
    }

	std::vector<ASTExpression*> args;
	NextToken(worker); // Eat the open paren
	while (worker->token.type != TOKEN_PAREN_CLOSE) {
		ASTExpression* expr = ParseExpr(worker);
		if (expr == nullptr) {
            ReportError(worker, &worker->token.site, "Could not resolve expression for argument at index %d in call to function named: %s", args.size(), identToken.string.c_str());
		} else {
			args.push_back(expr);
		}
	}
	NextToken(worker);		// Eat the close paren

	ASTCall* call = CreateCall(&worker->arena, &args[0], args.size(), identToken.string);
	return call;
}

ASTExpression* ParsePrimaryExpr(Worker* worker) {
	switch (worker->token.type) {
	case TOKEN_TRUE:
		NextToken(worker);
		return CreateIntegerLiteral(&worker->arena, 1);
		break;
	case TOKEN_FALSE:
		NextToken(worker);
		return CreateIntegerLiteral(&worker->arena, 0);
		break;

	case TOKEN_ADDRESS:
	case TOKEN_VALUE:
	case TOKEN_LOGIC_NOT:
	case TOKEN_IDENTIFIER: {
		UnaryOperator unary = UNARY_LOAD;
		if (worker->token.type == TOKEN_ADDRESS) {
			unary = UNARY_ADDRESS;
			NextToken(worker);	//Eat the pointer token
		} else if (worker->token.type == TOKEN_VALUE) {
			unary = UNARY_VALUE;
			NextToken(worker);	//Eat the deref token
		} else if (worker->token.type == TOKEN_LOGIC_NOT) {
			unary = UNARY_NOT;
			NextToken(worker);
		}

		Token identToken = worker->token;
		NextToken(worker); // Eat the identifier
		// TODO more robust error checking when receving statement tokens in an expression
        ASTNode* node;
		switch (worker->token.type) {
			case TOKEN_TYPE_DECLARE:
			case TOKEN_TYPE_DEFINE:
			case TOKEN_TYPE_INFER:
			case TOKEN_TYPE_RETURN:
				ReportError(worker, worker->token.site, "Unexpected token when parsing expression.	Token '" + worker->token.string + "' cannot be used in an expression");
				NextToken(worker);	// eat whatever that token was... hopefuly this will ha
				break;
			default:
                node = FindNodeWithIdent(worker->currentBlock, identToken.string);
				if (!node) {
					ReportError(worker, worker->token.site, "There is no node matching identifier: " + worker->token.string );
					NextToken (worker);
					return nullptr;
				}
				break;
		}

		if (worker->token.type == TOKEN_PAREN_OPEN) {
            if (node->nodeType == AST_DEFINITION || node->nodeType == AST_STRUCT) {
                auto cast = ParseCast(worker, identToken);
                return (ASTExpression*)cast;
            } else {
                auto call = ParseCall(worker, identToken);
                return (ASTExpression*)call;
            }
		} else if (worker->token.type == TOKEN_ACCESS) {


			auto parseMemberAccess = [](ASTVariable* structVar) {
				auto structType = (ASTStruct*)structVar->type;
				if (structType == nullptr) {
					std::vector<std::string> memberNames;
					while (worker->token.type == TOKEN_ACCESS) {
						NextToken(worker);
						if (worker->token.type != TOKEN_IDENTIFIER)
							ReportError(worker, &worker->token.site, "A struct member access must reference an identifier");
						memberNames.push_back(worker->token.string);
						NextToken(worker);
					}
				}
			}

			auto structVar = (ASTVariable*)node;
			auto structDefn = (ASTStruct*)structVar->type;
			std::vector<std::string> memberNames;
			if (structDefn == nullptr) {
				while (worker->token.type == TOKEN_ACCESS) {
					NextToken(worker);
					if (worker->token.type != TOKEN_IDENTIFIER)
						ReportError(worker, &worker->token.site, "A struct member access must reference an identifier");
					memberNames.push_back(worker->token.string);
					NextToken(worker);
				}

				//auto expr = CreateMemberExpr(&worker->arena, structVar, unary, &indices[0], indices.size());
				auto expr = CreateMemberExpr(&worker->arena, structVar, unary, memberNames);
				return expr;
			} else {
				if (structDefn->nodeType != AST_STRUCT) ReportError(worker, worker->token.site, identToken.string + " does not name a struct type!  It is a " + ToString(node->nodeType));
				auto currentStruct = structDefn;
				ASTDefinition* exprType = nullptr;
				std::vector<U32> indices;
				while(worker->token.type == TOKEN_ACCESS) {
					NextToken(worker); // eat the member access
					auto memberIndex = GetMemberIndex(currentStruct, worker->token.string);
					if (memberIndex == -1) {
						ReportError(worker, worker->token.site, worker->token.string + " does not name a member in struct '" + currentStruct->name + "'");
					} else {
						indices.push_back(memberIndex);
						auto memberType = currentStruct->members[memberIndex].type;
						if(memberType->nodeType == AST_STRUCT)
							currentStruct = (ASTStruct*)memberType;
						exprType = memberType;
					}
					NextToken(worker);	// eat the member ident
				}

				auto expr = CreateMemberExpr(&worker->arena, structVar, unary, memberNames);
				return expr;
			}




		} else {
			auto var = (ASTVariable*)node;
			auto expr = CreateVarExpr(&worker->arena, var, unary);
			return expr;
		}
		LOG_ERROR("SOMTHING TERRIBLE HAS HAPPENED!");
	} break;

	case TOKEN_PAREN_OPEN: {
		NextToken(worker); // Eat the open paren
		auto expr = ParseExpr(worker);
		if (worker->token.type != TOKEN_PAREN_CLOSE) {
			ReportError(worker, worker->token.site, "Expected close paren in expression");
		}
		NextToken(worker); // Eat the close paren
		return expr;
	} break;

		case TOKEN_NUMBER: {
				LOG_VERBOSE("Parsing a numberExpression!");
				auto dotPos = worker->token.string.find(".");
				bool isFloat = dotPos == std::string::npos ? false : true;
				if (isFloat) {
                    if(worker->token.string.substr(dotPos + 1).find(".") != std::string::npos) {
                            ReportError(worker, worker->token.site, "Floating Point value contains two decimal points!");
                    }
                    auto value = std::stof(worker->token.string);
                    auto result = CreateFloatLiteral(&worker->arena, value);
                    NextToken(worker); // Eat the float literal
                    return result;
				} else {
                    auto value = std::stoi(worker->token.string);
                    auto result = CreateIntegerLiteral(&worker->arena, value);
                    NextToken(worker); // Eat the int literal
                    return result;
				}
		} break;

		case TOKEN_STRING: {
			LOG_VERBOSE("Parsing a string expression...");
			auto str = CreateStringLiteral (&worker->arena, worker->token.string);
			NextToken(worker); // Eat the string token
			return str;
		} break;

		default:
            ReportError(worker, &worker->token.site, "Unkown token when expecting expression");
            NextToken(worker);
            return nullptr;
		}
		// This is dead code it will never happen.
}

// HACK there is a better way to do this but for now this works.
// This is nessecary because i dont want operations tied to the parsing of tokens
// because there may be suport for user defined operators in the future
// and intrinsic operations should be seperated from their tokens.
Operation tokenToOperation(const Token& token) {
    switch (token.type) {
    case TOKEN_ADD: return OPERATION_ADD;
    case TOKEN_SUB: return OPERATION_SUB;
    case TOKEN_MUL: return OPERATION_MUL;
    case TOKEN_DIV: return OPERATION_DIV;

    case TOKEN_LOGIC_GREATER: return OPERATION_GT;
    case TOKEN_LOGIC_LESS: return OPERATION_LT;
    case TOKEN_LOGIC_GREATER_EQAUL: return OPERATION_GTE;
    case TOKEN_LOGIC_LESS_EQUAL: return OPERATION_LTE;

    case TOKEN_LOGIC_OR: return OPERATION_LOR;
    case TOKEN_LOGIC_AND: return OPERATION_LAND;
    }
}

ASTExpression* ParseExprRHS (int exprPrec, ASTExpression* lhs, Worker* worker) {
	assert(lhs != nullptr);
	while (true) {
        // If the token prec is less than 0 that means that this is not a binary opperator
        // And we dont have to do anything aside from returning the allready parsed expression
        auto tokenPrec = GetTokenPrecedence(worker->token);
        if (tokenPrec < 0) {
            return lhs;
        }

        // We know that the currentToken is a binop
        // Possibly?	Yes!
        auto binopToken = worker->token;
        NextToken(worker);		// Eat the binop

        // We have a binop lets see what is on the other side!
        ASTExpression* rhs = ParsePrimaryExpr(worker);
        if (rhs == nullptr) {
            ReportError(worker, binopToken.site, "Could not parse primary expression to the right of binary opperator '" + binopToken.string	+ "'");
            return nullptr;
        }

        auto nextPrec = GetTokenPrecedence (worker->token);
        if (tokenPrec < nextPrec) {
            rhs = ParseExprRHS(tokenPrec + 1, rhs, worker);
            if (rhs == nullptr) {
                LOG_ERROR("Could not parse recursive rhsParsing!");
                return nullptr;
            }
        }

        lhs = (ASTExpression*)CreateBinaryOperation(&worker->arena, tokenToOperation(binopToken), lhs, rhs);
    }	 // Goes back to the while loop
}

ASTExpression* ParseExpr(Worker* worker) {
		auto lhs = ParsePrimaryExpr(worker);
		if (lhs == nullptr){
            return nullptr;
		}
		return ParseExprRHS (0, lhs, worker);
}

internal inline ASTNode* ParseIdentifier (Worker* worker) {
	auto identToken = worker->token;	// save the token for now
	auto node = FindNodeWithIdent(worker->currentBlock, identToken.string);
	NextToken(worker);	// Eat the identifier

	// The identifier that we are parsing may exist, may exit but be unresolved, or not exist at all
	// Depending on what we do with the identifer will determine if these factors matter
	switch(worker->token.type) {
	case TOKEN_TYPE_DECLARE: {
		if (node != nullptr) {
			ReportError(worker, identToken.site, " redefintion of identifier " + identToken.string + " first declared at site TODO SITE");
		}

		NextToken(worker); // Eat the ':'


		if (worker->token.type == TOKEN_ITER) {
			auto iter = ParseIter(worker, identToken.string);
			return iter;
		}

        // @VARIABLE
		else {
			auto var = CreateVariable(&worker->arena, worker->token.site, identToken.string.c_str());
            AssignIdent(worker->currentBlock, var, identToken.string);

			// TODO ParseUnaryOperators needs to handle double / triple / etc pointers
			// Or whatever other unary operators are defined!
			auto parseUnaryOperators = [&worker, &var]() {
				if (worker->token.type == TOKEN_ADDRESS) {
					var->isPointer = true;
					NextToken(worker);
				}
			};

			parseUnaryOperators();
			if (worker->token.type != TOKEN_IDENTIFIER) {
                if (worker->token.type == TOKEN_STRUCT) {
                    ReportError(worker, worker->token.site, "You accidently forgot the extra ':' when declaring a struct");
                } else {
                    ReportError(worker, worker->token.site, "type token '" + worker->token.string + "' is not an idnetifier!");
                }
			} else {
                auto type = (ASTDefinition*)FindNodeWithIdent(worker->currentBlock, worker->token.string);
				if (type == nullptr) {
					var->typeName = (char*)Allocate(&worker->arena, worker->token.string.size() + 1);
					memcpy(var->typeName, worker->token.string.c_str(), worker->token.string.size() + 1);
				} else if (type->nodeType != AST_DEFINITION && type->nodeType != AST_STRUCT) {
                    ReportError(worker, &worker->token.site, "%s does not name a type!  It names a %s", worker->token.string.c_str(), ToString(type->nodeType).c_str());
                } else {

				}

                var->type = type;
			}


			// Now we determine if this variable will be initalzied
			NextToken(worker); // eat type
			if (worker->token.type == TOKEN_EQUALS) {
				NextToken(worker);		//Eat the assignment operator
				var->initalExpression = ParseExpr(worker);	// We parse the inital expression for the variable!
				LOG_DEBUG(ident->site << "Identifier(" << ident->name << ") of Type(" << typeIdent->name << ") declared with an inital expression specified!");
			} else {
				LOG_DEBUG(ident->site << "Identifier(" << ident->name << ") of Type(" << typeIdent->name << ") declared!");
			}
			return var;
		}
	} break;


	case TOKEN_TYPE_INFER: {
		NextToken(worker);	// Eat the inference
		auto expr = ParseExpr(worker);
        if (node != nullptr) {
            ReportError(worker, identToken.site, "Redefinition of identifier: " + identToken.string);
            return nullptr;
        } else {
            auto var = CreateVariable(&worker->arena, worker->token.site, identToken.string.c_str(), expr);
            AssignIdent(worker->currentBlock, var, identToken.string);
            return var;
        }
	} break;

	case TOKEN_TYPE_DEFINE: {
		NextToken(worker); // Eat the typedef

        // @FUNCTION
		if (worker->token.type == TOKEN_PAREN_OPEN) {
			ASTFunctionSet* funcSet = (ASTFunctionSet*)node;
			if (funcSet == nullptr) {	// The identifier is null so the function set for this ident has not been created
                if (worker->currentBlock->parent == nullptr) {
                    worker->currentPackage->mutex.lock();
                    funcSet = CreateFunctionSet (&worker->currentPackage->arena);
                    AssignIdent(worker->currentBlock, funcSet, identToken.string);
                    worker->currentPackage->mutex.unlock();
                } else {
                    assert(false && "NOTE this will go terribly wrong if the worker isnt in the global scope");
                }
			}

            ASTFunction* function;
            if (worker->currentBlock->parent == nullptr) {
                worker->currentPackage->mutex.lock();
                function = CreateFunction(&worker->currentPackage->arena, worker->currentBlock, identToken.string, funcSet);
                worker->currentBlock->members.push_back(function);
                worker->currentPackage->mutex.unlock();
                worker->currentBlock = function;
                NextToken(worker);
            } else {
                assert(false && "lambdas not implemented!");
            }

			while (worker->token.type != TOKEN_PAREN_CLOSE) {
				if (worker->token.type == TOKEN_DOTDOT) {
                    function->isVarArgs = true;
                    NextToken(worker);
                    if (worker->token.type != TOKEN_PAREN_CLOSE) {
                        ReportError(worker, "VarArgs must appear at the end of the argument list");
                    } else {
                        break;
                    }
                }

                ASTNode* node = ParseStatement(worker);
				if (node == nullptr) {
					ReportError(worker, worker->token.site, "Could not parse arguments for function definition " + identToken.string);
				} else if (node->nodeType != AST_VARIABLE) {
					ReportError(worker, worker->token.site, "Function argument is not a varaibe!");
				} else {
					auto var = (ASTVariable*) node;
					function->args.push_back(var);
				}
			}

			NextToken(worker); // Eat the close ')'

			if (worker->token.type == TOKEN_TYPE_RETURN) {
				NextToken(worker);
				if (worker->token.type != TOKEN_IDENTIFIER) {
					LOG_ERROR("expected a type after the return operator");
					return nullptr;
				}

				function->returnType = (ASTDefinition*)FindNodeWithIdent(worker->currentBlock, worker->token.string);
				if (function->returnType == nullptr) {
					ReportError(worker, worker->token.site, "Could not resolve return type for function " + identToken.string);
				} else if (function->returnType->nodeType != AST_DEFINITION && function->returnType->nodeType != AST_STRUCT) {
					ReportError(worker, worker->token.site, "Identifier " + worker->token.string + " does not name a type");
				}

				NextToken(worker);
			}

			// There was no type return ':>' operator after the argument list but an expected token followed.
			// We assume it was intentional and that the return type is implicitly void
			else if (worker->token.type == TOKEN_BLOCK_OPEN || worker->token.type == TOKEN_FOREIGN) {
				function->returnType = global_voidType;
			} else {
				ReportError(worker, worker->token.site, "Expected new block or statemnt after function defininition");
			}

            ASTFunction* matchingFunction = nullptr;
            for (auto func : funcSet->functions) {
                bool foundMatchingFunction = true;
                if (func->args.size() == function->args.size()) {
                    for (U32 i = 0; i < func->args.size(); i++) {
                        if (func->args[i]->type != function->args[i]->type) {
                            foundMatchingFunction = false;
                            break;
                        }
                    }

                    if (foundMatchingFunction) {
                        matchingFunction = func;
                    }
                }
            }

            if (matchingFunction != nullptr && matchingFunction != function) {
			    ReportError(worker, &identToken.site, "Function %s has already been defined with arguments:", matchingFunction->name);
                printf("\t");
                for (U32 i = 0; i < matchingFunction->args.size(); i++) {
                    auto arg = matchingFunction->args[i];
                    printf("%s : %s", arg->name, arg->type->name);
                }
                printf("\n");
                if (matchingFunction->returnType != function->returnType) {
                    printf("Return types do not match!  Can not overload functions only by return type");
                }


            } else if (worker->token.type == TOKEN_BLOCK_OPEN) {
				ParseBlock(worker, function);

                // TODO change this to parseStatement to get the nextblock
//                // A new scope has been opened...
//                NextToken(worker); // Eat the scope
//
//                while (worker->token.type != TOKEN_BLOCK_CLOSE && worker->token.type != TOKEN_EOF) {
//                    // Here we are going to push back nullptrs into the function because they will never
//                    // Get to the codegenration phase anyway..	Instead of branching we can juse do this and not care
//                    ASTNode* node = ParseStatement(worker);
//                    function->members.push_back(node);
//                }

            } else if (worker->token.type != TOKEN_FOREIGN) {
                ReportError(worker, worker->token.site, "Expected a new scope to open after function definition!");
                LOG_INFO("Did you misspell foreign?");
                return nullptr;
            } else {
                if (function->parent->parent != nullptr) {
                    // This cant even happen there are no longer lambdas!
                    ReportError(worker, worker->token.site, "Cannot create a foreign function nested in another block!	Foreign functions must be declared in the global scope!");
                    return nullptr;
                }
				NextToken(worker);	// Eat the foreign token!
            }

            worker->currentBlock = function->parent;
            return function;
        }

	// @STRUCT
	else if (worker->token.type == TOKEN_STRUCT) {
		NextToken(worker); // Eat the struct keyword
		if (node != nullptr) {
			ReportError(worker, identToken.site, "Struct Redefinition");
		}

		if (worker->token.type == TOKEN_BLOCK_OPEN) {
			NextToken(worker); // Eat the open scope

            std::vector<ASTStructMember> members;
			while (worker->token.type != TOKEN_BLOCK_CLOSE) {
				if (worker->token.type != TOKEN_IDENTIFIER) {
					ReportError(worker, worker->token.site, "All statements inside structDefns must be identifier decls");
				}

				auto memberToken = worker->token; // Copy the ident token
				NextToken(worker); // Eat the identifier token;

				if (worker->token.type != TOKEN_TYPE_DECLARE) {
					ReportError(worker, worker->token.site, "All identifiers inside a struct defn must be member declarations!");
				}

				NextToken(worker); // Eat the typedecl
				bool isPointer = false;
				if (worker->token.type == TOKEN_ADDRESS) {
					isPointer = true;
					NextToken(worker);
				}

				auto typedefn = (ASTDefinition*)FindNodeWithIdent(worker->currentBlock, worker->token.string);
                if (typedefn == nullptr) {
                    ReportError(worker, worker->token.site, "Could not resolve type" + worker->token.string);
                }
				members.push_back(ASTStructMember());
                auto& structMember = members[members.size() - 1];
                // HACK
                structMember.name = (char*)Allocate(&worker->arena, memberToken.string.length() + 1);
                memcpy(structMember.name, memberToken.string.c_str(), memberToken.string.length() + 1);
				structMember.isPointer = isPointer;
				structMember.type = typedefn;

				NextToken(worker); // eat the type ident
			}

            NextToken(worker); // Eat the close scope

			if (members.size() > 0) {
                auto structDefn = CreateStruct(&worker->arena, identToken.string, &members[0], members.size());
                AssignIdent(worker->currentBlock, structDefn, identToken.string);
				worker->currentBlock->members.push_back(structDefn);
			} else {
				ReportError(worker, identToken.site, "Structs must contain at least one member");
			}
		} else {
			ReportError(worker, worker->token.site, "Structs must be defined with a block");
		}

		return ParseStatement(worker); // Consider the struct handeled and just get another node
		// Most of this requiring to return a node on parsing is a reminatnt of the epxression pased
		// functional stype language where everything is considered an expression
	} else {
		ReportError(worker, worker->token.site, "Could not define type with identifier: " + identToken.string +" (unknown keyword '" + worker->token.string + "')");
		NextToken(worker);
		return nullptr;
	}
} break;
	case TOKEN_PAREN_OPEN:	{
		LOG_VERBOSE("Attempting to parse call to : " << identToken.string);
		auto call = ParseCall(worker, identToken);
		return call;
	} break;

	case TOKEN_ACCESS: {
		if (node == nullptr) {
			ReportError(worker, identToken.site, "Could not resolve identifier " + identToken.string + " when trying to acess member");
		}

		// TODO namespace, enums, other things?
		auto structVar = (ASTVariable*)node;
		auto structDefn = (ASTStruct*)structVar->type;
		std::vector<std::string> memberNames;
		while (worker->token.type == TOKEN_ACCESS) {
			NextToken(worker);
			if (worker->token.type != TOKEN_IDENTIFIER)
				ReportError(worker, &worker->token.site, "A struct member access must reference an identifier");
			memberNames.push_back(worker->token.string);
			NextToken(worker);
		}


//		auto currentStruct = structDefn;
//		std::vector<U32> memberIndices;
//		while (worker->token.type == TOKEN_ACCESS) {
//			NextToken(worker);	// Eat the access token
//			if (worker->token.type != TOKEN_IDENTIFIER) ReportError(worker, worker->token.site, "Member access must reference an identifier");
//			auto memberIndex = GetMemberIndex(currentStruct, worker->token.string);
//			if (memberIndex == -1) {
//                ReportError(worker, worker->token.site, "");    // TODO fix these stupid log messages
//                printf("%s does not contain any member named %s", structDefn->name, worker->token.string.c_str());
//            }
//			memberIndices.push_back(memberIndex);
//
//			auto memberType = currentStruct->members[memberIndex].type;
//			if(memberType->nodeType == AST_STRUCT) currentStruct = (ASTStruct*)memberType;
//			NextToken(worker);	// Eat the member identifier
//		}

		Operation operation;
		switch (worker->token.type) {
		case TOKEN_EQUALS:			operation = OPERATION_ASSIGN; 	break;
		case TOKEN_ADD_EQUALS:	operation = OPERATION_ADD;		break;
		case TOKEN_SUB_EQUALS:	operation = OPERATION_SUB;		break;
		case TOKEN_MUL_EQUALS:	operation = OPERATION_MUL;		break;
		case TOKEN_DIV_EQUALS:	operation = OPERATION_DIV;		break;
		default: ReportError(worker, worker->token.site, "Unkown operator: " + worker->token.string);
		} NextToken(worker);

		auto expr = ParseExpr(worker);
		auto memberOperation = CreateMemberOperation(&worker->arena, structVar, operation, expr, memberNames);
		return memberOperation;
	} break;

	case TOKEN_EQUALS:
	case TOKEN_ADD_EQUALS:
	case TOKEN_SUB_EQUALS:
	case TOKEN_MUL_EQUALS:
	case TOKEN_DIV_EQUALS:
		Operation operation;
		switch (worker->token.type) {
		case TOKEN_EQUALS:			operation = OPERATION_ASSIGN; break;
		case TOKEN_ADD_EQUALS:	operation = OPERATION_ADD;		break;
		case TOKEN_SUB_EQUALS:	operation = OPERATION_SUB;		break;
		case TOKEN_MUL_EQUALS:	operation = OPERATION_MUL;		break;
		case TOKEN_DIV_EQUALS:	operation = OPERATION_DIV;		break;
		default: ReportError(worker, worker->token.site, "Unkown operator: " + worker->token.string);
		} NextToken(worker);	// Eat the operator

		auto var = (ASTVariable*)node;
		if (var == nullptr) {
			ReportError(worker, identToken.site, "Cannot create variable operation on unknown identifier(" + identToken.string + ")");
		}

		auto expr = ParseExpr(worker);
		if (expr == nullptr) {
			return nullptr;
		} else {
			return CreateVariableOperation(&worker->arena, var, operation, expr);
		}
	}

	assert(false && "THIS IS IMPOSSIBLEEE");
	return nullptr;
}

internal inline ASTNode* ParseIF(Worker* worker) {
	NextToken(worker);	// Eat the IF token
	auto expr = ParseExpr(worker);
	if (!expr) {
		ReportError(worker, worker->token.site, "Could not parse expresion for IF statement evaluation");
	}

	auto ifStatement = CreateIfStatement(&worker->arena, expr);
	auto body = ParseStatement(worker);
	ifStatement->ifBody = body;

	if (worker->token.type == TOKEN_ELSE) {
		NextToken(worker); //Eat the else keyword!
		auto elseBody = ParseStatement(worker);
		ifStatement->elseBody = elseBody;
	}
	return ifStatement;
}

internal inline ASTNode* ParseIter (Worker* worker, const std::string& identName) {
	LOG_VERBOSE(worker->token.site << "Parsing a iter statement");
	NextToken(worker); // Eat the iter

	auto expr = ParseExpr(worker);
	if (!expr) LOG_ERROR(worker->token.site << "Could not parse expression to the right of iter");

	if (worker->token.type == TOKEN_TO) {
		 NextToken(worker); 	// Eat the to
		 if (identName != "") {
			auto block = CreateBlock(&worker->arena, worker->currentBlock);
			auto var = CreateVariable(&worker->arena, worker->token.site, identName.c_str());
			var->type = global_S32Type;	// HACK
			var->initalExpression = expr;
			AssignIdent(block, var, identName);

			auto endExpr = ParseExpr(worker);
			if (!endExpr) LOG_ERROR(worker->token.site << "Could not parse Expression after TO keyword");
			ParseBlock(worker, block);
			auto iter = CreateIter(&worker->arena, var, expr, endExpr, nullptr, block);
			return iter;

		} else {
			ReportError(worker, worker->token.site, "iter statements that iterrate through a range must be declared with an identifier to hold the index!");
			ParseExpr(worker);	//eat the other expr
			//TODO skip block or somthing?
			return nullptr;
		}
	}

	else if (worker->token.type == TOKEN_BLOCK_OPEN) {
		auto block = ParseBlock(worker);
		LOG_ERROR("Not dealing with these for now");
	}
	return nullptr;
}

internal inline ASTNode* ParseBlock (Worker* worker, ASTBlock* block) {
	assert(worker->token.type == TOKEN_BLOCK_OPEN);
	NextToken(worker);	 // Eat the SCOPE_OPEN
	auto previousScope = worker->currentBlock;
	if (block == nullptr) {
		assert(false && "I dont think that this should ever happen!");
		block = CreateBlock(&worker->arena, previousScope);
	}

	worker->currentBlock = block;
	while (worker->token.type != TOKEN_BLOCK_CLOSE && worker->token.type != TOKEN_EOF) {
		auto node = ParseStatement(worker);
		block->members.push_back(node);
	}

	NextToken(worker);	// Eat the close scope
	worker->currentBlock = previousScope;
	return block;
}

void ParseFile (Worker* worker, const std::string& rootDir, const std::string& filename) {
	worker->file = fopen((rootDir + filename).c_str(), "r");
	if (!worker->file) {
		ReportError(worker, "Could not open file: " + filename);
		return;
	}

    auto currentErrorCount = worker->errorCount;
	worker->nextChar = getc(worker->file);
	worker->token.site.filename = filename;
	worker->lineNumber = 1;
	worker->colNumber = 1;
	worker->currentIndentLevel = 0;
	NextToken(worker);

	while (worker->token.type != TOKEN_EOF) {
		ParseStatement(worker);
	}

    auto errorCount = worker->errorCount - currentErrorCount;
	LOG_INFO("Parsed " << filename << (errorCount ? ("There were" + std::to_string(errorCount) + "errors") : ""));
	fclose(worker->file);
}
