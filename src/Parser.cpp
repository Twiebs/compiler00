#include <string.h>
#include <stdarg.h>

#include "Common.hpp"
#include "AST.hpp"
#include "Build.hpp"

#include "Lexer.hpp"
#include "Parser.hpp"
#include "ParserCommon.hpp"

void PushWork (const std::string& filename);

static ASTNode* ParseStatement(Worker* worker);
static ASTExpression* ParseExpr(Worker* worker);
static ASTNode* ParseReturn(Worker* worker);

static inline ASTNode* ParseIdentifier (Worker* worker);
static inline ASTNode* ParseIF (Worker* worker);
static inline ASTNode* ParseIter (Worker* worker, const std::string& identName = "");
static inline ASTNode* ParseBlock (Worker* worker, ASTBlock* block = nullptr);

void ReportError (Worker* worker, FileSite* site, const char* msg, ...);
void ReportError (Worker* worker, const FileSite& site, const std::string& msg);
void ReportError (Worker* worker, const std::string& msg);

void ReportError (Worker* worker, const FileSite& site, const std::string& msg) {
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

ASTNode* ParseImport (Worker* worker) {
	worker->lex.nextToken(); // Eat the import statement
	if (worker->lex.token.type != TOKEN_STRING) {
		ReportError(worker, worker->lex.token.site, "Import keyword requires a string to follow it");
		worker->lex.eatLine();	// TODO this is a minor hack ...
	}
	else {
		PushWork(worker->lex.token.string);
	}
	worker->lex.nextToken();	 // Eat the import string
	return ParseStatement(worker);
}

static ASTNode* ParseTopLevelStatement(Worker* worker) {
	switch (worker->lex.token.type) {
	case TOKEN_ADD:
	case TOKEN_SUB:
	case TOKEN_MUL:
	case TOKEN_DIV:
	case TOKEN_CONSTRUCT:
	case TOKEN_IDENTIFIER:	      	return ParseIdentifier(worker);
	case TOKEN_IMPORT:			  	return ParseImport(worker);

	default:
		ReportError(worker, &worker->lex.token.site, "Could not parse toplevel statement begining with token type %s with source code: %s", ToString(worker->lex.token.type), worker->lex.token.string.c_str());
		worker->lex.nextToken();
		return nullptr;
	}
}

ASTNode* ParseStatement (Worker* worker) {
	switch (worker->lex.token.type) {

    case TOKEN_ADD:
	case TOKEN_SUB:
	case TOKEN_MUL:
	case TOKEN_DIV:
    case TOKEN_CONSTRUCT:
        

	case TOKEN_IDENTIFIER:	      	return ParseIdentifier(worker);
	case TOKEN_IF: 					return ParseIF(worker);
	case TOKEN_ITER:				return ParseIter(worker);
	case TOKEN_RETURN: 			  	return ParseReturn(worker);
	case TOKEN_BLOCK_OPEN:	      	return ParseBlock(worker);
	case TOKEN_IMPORT:			  	return ParseImport(worker);
	default:
		ReportError(worker, &worker->lex.token.site, "Could not parse statement: Unknown token(%s)", worker->lex.token.string.c_str());
		worker->lex.nextToken();
		return nullptr;
	}
}

static void SkipEntireBlock (Worker* worker) {
    int blockLevel = 1;
    while (blockLevel > 0) {
        if (worker->lex.token.type == TOKEN_BLOCK_OPEN)
            blockLevel++;
        else if (worker->lex.token.type == TOKEN_BLOCK_CLOSE)
            blockLevel--;
        worker->lex.nextToken();
    }
}

ASTNode* ParseReturn (Worker* worker) {
		worker->lex.nextToken();
		auto expr = ParseExpr(worker);
		auto returnValue = worker->arena.alloc<ASTReturn>(expr, worker->lex.token.site);
		return returnValue;
}

static inline ASTCast* ParseCast (Worker* worker, const Token& identToken) {
    worker->lex.nextToken();
    auto typeDefn = (ASTDefinition*)FindNodeWithIdent(worker->currentBlock, identToken.string);
    assert(typeDefn->nodeType == AST_DEFINITION);
    auto expr = ParseExpr(worker);

    if (worker->lex.token.type != TOKEN_PAREN_CLOSE) {
        ReportError(worker, &worker->lex.token.site, "expected close parren enlosing expression when casting to type %s", identToken.string.c_str());
        return nullptr;
    } else {
        worker->lex.nextToken();
        auto cast = CreateCast(&worker->arena, typeDefn, expr);
        return cast;
    }
}

ASTCall* ParseCall (Worker* worker, const Token& identToken) {
    if (worker->currentBlock->parent == nullptr) {
        ReportError(worker, worker->lex.token.site, "Can not call functions outside a block!  Did you mean to use :: ?");
    }

	std::vector<ASTExpression*> args;
	worker->lex.nextToken(); // Eat the open paren
	while (worker->lex.token.type != TOKEN_PAREN_CLOSE) {
		ASTExpression* expr = ParseExpr(worker);
		if (expr == nullptr) {
            ReportError(worker, &worker->lex.token.site, "Could not resolve expression for argument at index %d in call to function named: %s", args.size(), identToken.string.c_str());
		} else {
			args.push_back(expr);
		}
	}
	worker->lex.nextToken();		// Eat the close paren

	ASTCall* call = CreateCall(&worker->arena, &args[0], args.size(), identToken.string);
	return call;
}


static void ParseMemberAccess (Worker* worker, ASTVariable* structVar, ASTMemberAccess* memberAccess) {
	auto structType = (ASTStruct*)structVar->type;
	if (structType == nullptr) {
		std::vector<char*> memberNames;
		while (worker->lex.token.type == TOKEN_ACCESS) {
			worker->lex.nextToken();
			if (worker->lex.token.type != TOKEN_IDENTIFIER)
				ReportError(worker, &worker->lex.token.site, "A struct member access must reference an identifier");
			memberNames.push_back((char*)Allocate(&worker->arena, worker->lex.token.string.length() + 1));
			worker->lex.nextToken();
		}
		memberAccess->memberCount = memberNames.size();
		memberAccess->memberNames = (char**)Allocate(&worker->arena, memberNames.size() * sizeof(char*));
		memberAccess->indices = (U32*)Allocate(&worker->arena, memberNames.size() * sizeof(U32));
		memcpy(memberAccess->memberNames, &memberNames.front(), memberNames.size() * sizeof(char*));
	} else {

		std::vector<U32> indices;
		auto currentStruct = structType;
		while (worker->lex.token.type == TOKEN_ACCESS) {
			worker->lex.nextToken(); // eat the member access
			auto memberIndex = GetMemberIndex(currentStruct, worker->lex.token.string);
			if (memberIndex == -1) {
				ReportError(worker, worker->lex.token.site, worker->lex.token.string + " does not name a member in struct '" + currentStruct->name + "'");
			} else {
				auto memberType = currentStruct->members[memberIndex].type;
				if(memberType->nodeType == AST_STRUCT)
					currentStruct = (ASTStruct*)memberType;
			}
			indices.push_back(memberIndex);
			worker->lex.nextToken();	// eat the member ident
		}
		memberAccess->memberCount = indices.size();
		memberAccess->indices = (U32*)Allocate(&worker->arena, indices.size() * sizeof(U32));
		memberAccess->memberNames = nullptr;
		memcpy(memberAccess->indices, &indices.front(), indices.size() * sizeof(U32));
	}
};

static inline ASTUnaryOp* ParseUnaryOperation (Worker* worker) {
	auto& lex = worker->lex;

	auto getIndirectionLevel = [&lex](TokenType indirectionOperator, S8 increment) {
		S8 indirectionLevel = 0;
		while (lex.token.type == indirectionOperator) {
			indirectionLevel += increment;
			lex.nextToken();
		}
		return indirectionLevel;
	};

	S8 indirectionLevel = 0;
	if (lex.token.type == TOKEN_ADDRESS) {
		indirectionLevel += getIndirectionLevel(TOKEN_ADDRESS, 1);
	} else if (lex.token.type == TOKEN_VALUE) {
		indirectionLevel += getIndirectionLevel(TOKEN_VALUE, -1);
	} else {
		assert(false);
	}

	if (IsUnaryOperator(lex.token.type)) {
		ReportError(worker, &lex.token.site, "Cannot parse two different types of unary opeartors in the same expression!");
		while (IsUnaryOperator(lex.token)) {
			lex.nextToken();
		}
	}

	auto expr = ParseExpr(worker);
	auto unaryOperation = worker->arena.alloc<ASTUnaryOp>(UNARY_INDIRECTION, expr);
	return unaryOperation;
}

ASTExpression* ParsePrimaryExpr(Worker* worker) {
	switch (worker->lex.token.type) {
	case TOKEN_ADDRESS:
	case TOKEN_VALUE:
	case TOKEN_LOGIC_NOT:
	{
		return ParseUnaryOperation(worker);
	}
	// TODO don't recreate true and false literals every time one is required
	case TOKEN_TRUE:
	{
		worker->lex.nextToken();
		return CreateIntegerLiteral(&worker->arena, 1);
	}
	case TOKEN_FALSE:
	{
		worker->lex.nextToken();
		return CreateIntegerLiteral(&worker->arena, 0);
	}

	
	case TOKEN_IDENTIFIER: {

		Token identToken = worker->lex.token;
		worker->lex.nextToken(); // Eat the identifier
		// TODO more robust error checking when receving statement tokens in an expression
        ASTNode* node;
		switch (worker->lex.token.type) {
			case TOKEN_TYPE_DECLARE:
			case TOKEN_TYPE_DEFINE:
			case TOKEN_TYPE_INFER:
			case TOKEN_TYPE_RETURN:
				ReportError(worker, worker->lex.token.site, "Unexpected token when parsing expression.	Token '" + worker->lex.token.string + "' cannot be used in an expression");
				worker->lex.nextToken();	// eat whatever that token was... hopefuly this will ha
				break;
			default:
                node = FindNodeWithIdent(worker->currentBlock, identToken.string);
				if (node == nullptr) {
					ReportError(worker, &identToken.site, "There is no expression matching the identifier (%s)", identToken.string.c_str());
					worker->lex.nextToken();
					return nullptr;
				}
				break;
		}

		if (worker->lex.token.type == TOKEN_PAREN_OPEN) {
            if (node->nodeType == AST_DEFINITION || node->nodeType == AST_STRUCT) {
                auto cast = ParseCast(worker, identToken);
                return (ASTExpression*)cast;
            } else {
                auto call = ParseCall(worker, identToken);
                return (ASTExpression*)call;
            }
		} else if (worker->lex.token.type == TOKEN_ACCESS) {
			auto structVar = (ASTVariable*)node;
			auto structDefn = (ASTStruct*)structVar->type;
			auto expr = CreateMemberExpr(&worker->arena, structVar);
			ParseMemberAccess(worker, structVar, &expr->access);
			return expr;

//			if (structDefn == nullptr) {
//				while (worker->lex.token.type == TOKEN_ACCESS) {
//					worker->lex.nextToken();
//					if (worker->lex.token.type != TOKEN_IDENTIFIER)
//						ReportError(worker, &worker->lex.token.site, "A struct member access must reference an identifier");
//					memberNames.push_back(worker->lex.token.string);
//					worker->lex.nextToken();
//				}
//
//				//auto expr = CreateMemberExpr(&worker->arena, structVar, unary, &indices[0], indices.size());
//				auto expr = CreateMemberExpr(&worker->arena, structVar, unary, memberNames);
//				return expr;
////			} else {
//				if (structDefn->nodeType != AST_STRUCT) ReportError(worker, worker->lex.token.site, identToken.string + " does not name a struct type!  It is a " + ToString(node->nodeType));
//				auto currentStruct = structDefn;
//				ASTDefinition* exprType = nullptr;
//				std::vector<U32> indices;
//				while(worker->lex.token.type == TOKEN_ACCESS) {
//					worker->lex.nextToken(); // eat the member access
//					auto memberIndex = GetMemberIndex(currentStruct, worker->lex.token.string);
//					if (memberIndex == -1) {
//						ReportError(worker, worker->lex.token.site, worker->lex.token.string + " does not name a member in struct '" + currentStruct->name + "'");
//					} else {
//						indices.push_back(memberIndex);
//						auto memberType = currentStruct->members[memberIndex].type;
//						if(memberType->nodeType == AST_STRUCT)
//							currentStruct = (ASTStruct*)memberType;
//						exprType = memberType;
//					}
//					worker->lex.nextToken();	// eat the member ident
//				}
//
//				auto expr = CreateMemberExpr(&worker->arena, structVar, unary, memberNames);
//				return expr;
//			}




		} else {
			auto var = static_cast<ASTVariable*>(node);
			auto expr = worker->arena.alloc<ASTVarExpr>(var);
			return expr;
		}
		LOG_ERROR("SOMTHING TERRIBLE HAS HAPPENED!");
	} break;

	case TOKEN_PAREN_OPEN: {
		worker->lex.nextToken(); // Eat the open paren
		auto expr = ParseExpr(worker);
		if (worker->lex.token.type != TOKEN_PAREN_CLOSE) {
			ReportError(worker, worker->lex.token.site, "Expected close paren in expression");
		}
		worker->lex.nextToken(); // Eat the close paren
		return expr;
	} break;

		case TOKEN_NUMBER: {
				LOG_VERBOSE("Parsing a numberExpression!");
				auto dotPos = worker->lex.token.string.find(".");
				bool isFloat = dotPos == std::string::npos ? false : true;
				if (isFloat) {
                    if(worker->lex.token.string.substr(dotPos + 1).find(".") != std::string::npos) {
                            ReportError(worker, worker->lex.token.site, "Floating Point value contains two decimal points!");
                    }
                    auto value = std::stof(worker->lex.token.string);
                    auto result = CreateFloatLiteral(&worker->arena, value);
                    worker->lex.nextToken(); // Eat the float literal
                    return result;
				} else {
                    auto value = std::stoi(worker->lex.token.string);
                    auto result = CreateIntegerLiteral(&worker->arena, value);
                    worker->lex.nextToken(); // Eat the int literal
                    return result;
				}
		}

		case TOKEN_STRING: {
			auto str = CreateStringLiteral (&worker->arena, worker->lex.token.string);
			worker->lex.nextToken(); // Eat the string token
			return str;
		}

		default:
			ReportSourceError(worker->lex.token.location, "Error when expecting expression: Unkown Token(" << worker->lex.token.string.c_str());
            worker->lex.nextToken();
            return nullptr;
		}
		// This is dead code it will never happen.
}



ASTExpression* ParseExprRHS (int exprPrec, ASTExpression* lhs, Worker* worker) {
	assert(lhs != nullptr);
	while (true) {
        // If the token prec is less than 0 that means that this is not a binary opperator
        // And we dont have to do anything aside from returning the allready parsed expression
        auto tokenPrec = GetTokenPrecedence(worker->lex.token);
        if (tokenPrec < 0) {
            return lhs;
        }

        // We know that the currentToken is a binop
        // Possibly?	Yes!
        auto binopToken = worker->lex.token;
        worker->lex.nextToken();		// Eat the binop

        // We have a binop lets see what is on the other side!
        ASTExpression* rhs = ParsePrimaryExpr(worker);
        if (rhs == nullptr) {
            ReportError(worker, binopToken.site, "Could not parse primary expression to the right of binary opperator '" + binopToken.string	+ "'");
            return nullptr;
        }

        auto nextPrec = GetTokenPrecedence (worker->lex.token);
        if (tokenPrec < nextPrec) {
            rhs = ParseExprRHS(tokenPrec + 1, rhs, worker);
            if (rhs == nullptr) {
                LOG_ERROR("Could not parse recursive rhsParsing!");
                return nullptr;
            }
        }

        lhs = (ASTExpression*)CreateBinaryOperation(&worker->arena, TokenToOperation(binopToken), lhs, rhs);
    }	 // Goes back to the while loop
}

ASTExpression* ParseExpr(Worker* worker) {
		auto lhs = ParsePrimaryExpr(worker);
		if (lhs == nullptr){
            return nullptr;
		}
		return ParseExprRHS (0, lhs, worker);
}

static inline bool IsBinaryOperator(TokenType type) {
    switch (type) {
        case TOKEN_ADD:
        case TOKEN_SUB:
        case TOKEN_MUL:
        case TOKEN_DIV:
        case TOKEN_CONSTRUCT:
            return true;
        default:
            return false;
    }
}

static void ExpectAndEatToken (Worker* worker, TokenType expectedToken) {
    if (worker->lex.token.type != expectedToken) {
        ReportError(worker, &worker->lex.token.site, "Expected token: %s");
        return;
    }

    worker->lex.nextToken();
}

static void ExpectToken(Worker* worker, TokenType expectedToken) {
    if (worker->lex.token.type != expectedToken) {
        ReportError(worker, &worker->lex.token.site, "Expected token: %s");
        return;
    }
}



static void ParseOperatorOverload(Worker* worker) {
    auto& lex = worker->lex;
    assert(lex.token.type == TOKEN_CONSTRUCT && "Only construction operator supported currently");
    ExpectAndEatToken(worker, TOKEN_TYPE_DEFINE);
    ExpectToken(worker, TOKEN_PAREN_OPEN);

}

//static void ParseTopLevelStatement (Worker* worker) {
//    auto& lex = worker->lex;
//    if (lex.token.type == TOKEN_IDENTIFIER)
//        ParseIdentifier(worker);
//    else if (IsBinaryOperator(lex.token.type)) {
//        ParseOperatorOverload(worker);
//    }
//}

static inline ASTVariable* ParseVariableDecleration (Worker* worker, const Token& identToken) {

	// TODO ParseUnaryOperators needs to handle double / triple / etc pointers
	// Or whatever other unary operators are defined!

	static auto ParseIndirectionLevel = [](Worker* worker) -> S8 {
		auto& lex = worker->lex;
		int indirectionLevel = 0;
		while (worker->lex.token.type == TOKEN_ADDRESS) {
			indirectionLevel++;
			lex.nextToken();
		}

		if (lex.token.type != TOKEN_IDENTIFIER) {
			ReportError(worker, lex.token.site, "An identifier must folow indeirection qualifiers in a variable decleration!");
		}

		return indirectionLevel;
	};

	S8 indirectionLevel = ParseIndirectionLevel(worker);
	auto varDecl = CreateVariable(&worker->arena, identToken.site, identToken.string, nullptr, indirectionLevel);
	AssignIdent(worker->currentBlock, varDecl, identToken.string);

	if (worker->lex.token.type != TOKEN_IDENTIFIER) {
		if (worker->lex.token.type == TOKEN_STRUCT) {
			ReportError(worker, worker->lex.token.site, "You accidently forgot the extra ':' when declaring a struct");
		} else {
			ReportError(worker, worker->lex.token.site, "type lex.token '" + worker->lex.token.string + "' is not an idnetifier!");
		}
	} else {
		auto type = (ASTDefinition*)FindNodeWithIdent(worker->currentBlock, worker->lex.token.string);
		if (type == nullptr) {
			varDecl->typeName = (char*)Allocate(&worker->arena, worker->lex.token.string.size() + 1);
			memcpy(varDecl->typeName, worker->lex.token.string.c_str(), worker->lex.token.string.size() + 1);
		} else if (type->nodeType != AST_DEFINITION && type->nodeType != AST_STRUCT) {
			ReportError(worker, &worker->lex.token.site, "%s does not name a type!  It names a %s", worker->lex.token.string.c_str(), ToString(type->nodeType).c_str());
		} else {
			varDecl->type = type;
		}
	}


	// Now we determine if this variable will be initalzied
	worker->lex.nextToken(); // eat type
	if (worker->lex.token.type == TOKEN_EQUALS) {
		worker->lex.nextToken();		//Eat the assignment operator
		varDecl->initalExpression = ParseExpr(worker);	// We parse the inital expression for the variable!
		LOG_DEBUG(ident->site << "Identifier(" << ident->name << ") of Type(" << typeIdent->name << ") declared with an inital expression specified!");
	} else {
		LOG_DEBUG(ident->site << "Identifier(" << ident->name << ") of Type(" << typeIdent->name << ") declared!");
	}
	return varDecl;
}

// This function does not return a ASTStruct
// It skips the node and moves and returns the next one
static inline ASTNode* ParseStructDefn(Worker* worker, const Token& identToken, ASTNode* node) {
	auto& lex = worker->lex;
	assert(lex.token.type == TOKEN_STRUCT);
	lex.nextToken(); // Eat the struct keyword
	if (node != nullptr) {
		ReportError(worker, identToken.site, "Struct Redefinition");
	}

	if (worker->lex.token.type == TOKEN_BLOCK_OPEN) {
		worker->lex.nextToken();

		std::vector<ASTStructMember> members;
		while (worker->lex.token.type != TOKEN_BLOCK_CLOSE) {
			if (worker->lex.token.type != TOKEN_IDENTIFIER) {
				ReportError(worker, worker->lex.token.site, "All statements inside structDefns must be identifier decls");
			}

			auto memberToken = worker->lex.token; // Copy the ident lex.token
			worker->lex.nextToken(); // Eat the identifier lex.token;

			if (worker->lex.token.type != TOKEN_TYPE_DECLARE) {
				ReportError(worker, worker->lex.token.site, "All identifiers inside a struct defn must be member declarations!");
			}

			worker->lex.nextToken(); // Eat the typedecl
			bool isPointer = false;
			if (worker->lex.token.type == TOKEN_ADDRESS) {
				isPointer = true;
				worker->lex.nextToken();
			}

			auto typedefn = (ASTDefinition*)FindNodeWithIdent(worker->currentBlock, worker->lex.token.string);
			if (typedefn == nullptr) {
				ReportError(worker, worker->lex.token.site, "Could not resolve type" + worker->lex.token.string);
			}
			members.push_back(ASTStructMember());
			auto& structMember = members[members.size() - 1];
			// HACK
			structMember.name = (char*)Allocate(&worker->arena, memberToken.string.length() + 1);
			memcpy(structMember.name, memberToken.string.c_str(), memberToken.string.length() + 1);
			structMember.isPointer = isPointer;
			structMember.type = typedefn;

			worker->lex.nextToken(); // eat the type ident
		}

		worker->lex.nextToken(); // Eat the close scope

		if (members.size() > 0) {
			auto structDefn = CreateStruct(&worker->arena, identToken.site, identToken.string, &members[0], members.size());
			AssignIdent(worker->currentBlock, structDefn, identToken.string);
			worker->currentBlock->members.push_back(structDefn);
		} else {
			ReportError(worker, identToken.site, "Structs must contain at least one member");
		}
	} else {
		ReportError(worker, worker->lex.token.site, "Structs must be defined with a block");
	}

	return ParseStatement(worker);
}

static inline ASTFunction* ParseFunction(Worker* worker, ASTFunctionSet* functionSet, const Token& identToken) {
		std::function<ASTFunctionSet*(ASTBlock*)> getOrCreateFunctionSet = [&worker, &identToken, &getOrCreateFunctionSet](ASTBlock* block) -> ASTFunctionSet* {
		if (block->parent == nullptr) {
			worker->currentPackage->mutex.lock();
			auto functionSet = worker->currentPackage->arena.alloc<ASTFunctionSet>(nullptr);
			AssignIdent(block, functionSet, identToken.string);
			worker->currentPackage->mutex.unlock();
			return functionSet;
		} else {
			auto functionSet = (ASTFunctionSet*)FindNodeWithIdent(block, identToken.string);
			if (functionSet == nullptr) {
				auto parentFunctionSet = getOrCreateFunctionSet(block->parent);
				functionSet = worker->arena.alloc<ASTFunctionSet>(parentFunctionSet);
				AssignIdent(block, functionSet, identToken.string);
			}
			return functionSet;
		}
	};


	if (functionSet == nullptr) {
		functionSet = getOrCreateFunctionSet(worker->currentBlock);
	} else if (functionSet->nodeType != AST_FUNCTION_SET) {
		ReportSourceError(identToken.location, "This is not a functionset");
		return nullptr;
	}



	//			if (funcSet == nullptr) {	// The identifier is null so the function set for this ident has not been created
	//                if (worker->currentBlock->parent == nullptr) {
	//                    worker->currentPackage->mutex.lock();
	//                    funcSet = worker->currentPackage->arena.alloc<ASTFunctionSet>(nullptr);
	//                    AssignIdent(worker->currentBlock, funcSet, identToken.string);
	//                    worker->currentPackage->mutex.unlock();
	//                } else {
	//					ASTFunctionSet* parentFunctionSet = (ASTFunctionSet*)FindNodeWithIdent(worker->currentBlock->parent, identToken.string);
	//					funcSet = worker->arena.alloc<ASTFunctionSet>(parentFunctionSet);
	//					AssignIdent(worker->currentBlock, funcSet, identToken.string);
	//                }
	//			}

	ASTFunction* function;
	if (worker->currentBlock->parent == nullptr) {
		worker->currentPackage->mutex.lock();
		function = CreateFunction(&worker->currentPackage->arena, worker->currentBlock, identToken.string, functionSet);
		worker->currentBlock->members.push_back(function);
		worker->currentPackage->mutex.unlock();
		worker->currentBlock = function;
	} else {
		function = CreateFunction(&worker->arena, worker->currentBlock, identToken.string, functionSet);
	}

	worker->lex.nextToken();

	while (worker->lex.token.type != TOKEN_PAREN_CLOSE) {
		if (worker->lex.token.type == TOKEN_DOTDOT) {
			function->isVarArgs = true;
			worker->lex.nextToken();
			if (worker->lex.token.type != TOKEN_PAREN_CLOSE) {
				ReportError(worker, "VarArgs must appear at the end of the argument list");
			} else {
				break;
			}
		}

		ASTNode* node = ParseStatement(worker);
		if (node == nullptr) {
			ReportError(worker, worker->lex.token.site, "Could not parse arguments for function definition " + identToken.string);
		} else if (node->nodeType != AST_VARIABLE) {
			ReportError(worker, worker->lex.token.site, "Function argument is not a varaibe!");
		} else {
			auto var = (ASTVariable*)node;
			function->args.push_back(var);
		}
	}

	worker->lex.nextToken(); // Eat the close ')'

	if (worker->lex.token.type == TOKEN_TYPE_RETURN) {
		worker->lex.nextToken();
		if (worker->lex.token.type != TOKEN_IDENTIFIER) {
			LOG_ERROR("expected a type after the return operator");
			return nullptr;
		}

		function->returnType = (ASTDefinition*)FindNodeWithIdent(worker->currentBlock, worker->lex.token.string);
		if (function->returnType == nullptr) {
			ReportError(worker, worker->lex.token.site, "Could not resolve return type for function " + identToken.string);
		} else if (function->returnType->nodeType != AST_DEFINITION && function->returnType->nodeType != AST_STRUCT) {
			ReportError(worker, worker->lex.token.site, "Identifier " + worker->lex.token.string + " does not name a type");
		}

		worker->lex.nextToken();
	}

	// There was no type return ':>' operator after the argument list but an expected lex.token followed.
	// We assume it was intentional and that the return type is implicitly void
	else if (worker->lex.token.type == TOKEN_BLOCK_OPEN || worker->lex.token.type == TOKEN_FOREIGN) {
		function->returnType = global_voidType;
	} else {
		ReportError(worker, worker->lex.token.site, "Expected new block or statemnt after function defininition");
	}


	std::function<ASTFunction*(ASTFunction*, ASTFunctionSet*, ASTBlock*)> CheckFunctionArgumentCollision = [&worker, &CheckFunctionArgumentCollision](ASTFunction* createdFunction, ASTFunctionSet* functionSet, ASTBlock* currentBlock) ->ASTFunction* {
		for (auto func : functionSet->functions) {
			bool foundMatchingFunction = true;
			if (func->args.size() == createdFunction->args.size()) {
				for (U32 i = 0; i < func->args.size(); i++) {
					if (func->args[i]->type != createdFunction->args[i]->type) {
						foundMatchingFunction = false;
						break;
					}
				}

				if (foundMatchingFunction) {
					return func;
				} else if (currentBlock->parent != nullptr) {
					auto parentFunctionSet = (ASTFunctionSet*)FindNodeWithIdent(currentBlock->parent, createdFunction->name);
					if (parentFunctionSet->nodeType != AST_FUNCTION_SET) {
						ReportError(worker, &worker->lex.token.site, "Identifier in blocks parrent scope was not a function!");
					} else if (parentFunctionSet != nullptr) {
						return CheckFunctionArgumentCollision(createdFunction, parentFunctionSet, currentBlock->parent);
					}
				}
			}
		}
		return nullptr;
	};


	ASTFunction* matchingFunction = CheckFunctionArgumentCollision(function, functionSet, worker->currentBlock);
	if (matchingFunction != nullptr && matchingFunction != function) {
		ReportSourceError(identToken.location, "Function " << matchingFunction->name << " has already been defined with arguments: \t");
		for (U32 i = 0; i < matchingFunction->args.size(); i++) {
			auto arg = matchingFunction->args[i];
			printf("%s : %s", arg->name, arg->type->name);
		}
		printf("\n");
		if (matchingFunction->returnType != function->returnType) {
			printf("Return types do not match!  Can not overload functions only by return type");
		}
	} else if (worker->lex.token.type == TOKEN_BLOCK_OPEN) {
		ParseBlock(worker, function);

		// TODO change this to parseStatement to get the nextblock
		//                // A new scope has been opened...
		//                worker->lex.nextToken(); // Eat the scope
		//
		//                while (worker->lex.token.type != TOKEN_BLOCK_CLOSE && worker->lex.token.type != TOKEN_EOF) {
		//                    // Here we are going to push back nullptrs into the function because they will never
		//                    // Get to the codegenration phase anyway..	Instead of branching we can juse do this and not care
		//                    ASTNode* node = ParseStatement(worker);
		//                    function->members.push_back(node);
		//                }

	} else if (worker->lex.token.type != TOKEN_FOREIGN) {
		ReportError(worker, worker->lex.token.site, "Expected a new scope to open after function definition!");
		LOG_INFO("Did you misspell foreign?");
		return nullptr;
	} else {
		if (function->parent->parent != nullptr) {
			// This cant even happen there are no longer lambdas!
			ReportError(worker, worker->lex.token.site, "Cannot create a foreign function nested in another block!	Foreign functions must be declared in the global scope!");
			return nullptr;
		}
		worker->lex.nextToken();	// Eat the foreign lex.token!
	}

	worker->currentBlock = function->parent;
	return function;


}

static inline ASTMemberOperation* ParseMemberOperation(Worker* worker, ASTVariable* structVariable) {
	auto lex = &worker->lex;
	if (structVariable == nullptr) {
		ReportSourceError(structVariable->sourceLocation, "Could not resolve identifier " << structVariable->name << " when trying to acess member");
	}

	// TODO namespace, enums, other things?
	auto structDefn = static_cast<ASTStruct*>(structVariable->type);
	assert(structDefn->nodeType == AST_STRUCT);

	SourceLocation loc;
	auto memberOperation = worker->arena.alloc<ASTMemberOperation>(loc, structVariable, nullptr, (Operation)0);
	memberOperation->sourceLocation.lineNumber = worker->lex.token.site.lineNumber;
	memberOperation->sourceLocation.columnNumber = worker->lex.token.site.columNumber;
	memberOperation->sourceLocation.filename = worker->currentPackage->name.c_str();
	ParseMemberAccess(worker, structVariable, &memberOperation->access);
	memberOperation->operation = TokenToOperation(worker->lex.token);
	lex->nextToken(); // eat the operation token
	auto expr = ParseExpr(worker);
	memberOperation->expr = expr;
	return memberOperation;
}

#define DEBUG_MAX_WORKER_COUNT 4
#define DEBUG_DECLARE_CALL_COUNTER() static U32 __debugCallCounter[DEBUG_MAX_WORKER_COUNT]
#define DEBUG_CALL_COUNTER_VALUE __debugCallCounter[worker->workerID]
#define DEBUG_INCREMENT_CALL_COUNTER() DEBUG_CALL_COUNTER_VALUE++;
#define DEBUG_CALL_COUNTER() DEBUG_DECLARE_CALL_COUNTER(); DEBUG_INCREMENT_CALL_COUNTER()
static inline ASTNode* ParseIdentifier (Worker* worker) {
	DEBUG_CALL_COUNTER();


	auto identToken = worker->lex.token;	//Copy the identToken for later use
	worker->lex.nextToken();
	auto node = FindNodeWithIdent(worker->currentBlock, identToken.string);

	// The identifier that we are parsing may exist, may exit but be unresolved, or not exist at all
	// Depending on what we do with the identifer will determine if these factors matter
	switch(worker->lex.token.type) {

	case TOKEN_TYPE_DECLARE: {
		if (node != nullptr) {
			ReportError(worker, identToken.site, " redefintion of identifier " + identToken.string + " first declared at site TODO SITE");
		}

		worker->lex.nextToken(); // Eat the ':'


		if (worker->lex.token.type == TOKEN_ITER) {
			auto iter = ParseIter(worker, identToken.string);
			return iter;
		}

        // @VARIABLE
		else {
			auto varDecl = ParseVariableDecleration(worker, identToken);
			return varDecl;
		}
	} break;


	case TOKEN_TYPE_INFER: {
		worker->lex.nextToken();	// Eat the inference
		auto expr = ParseExpr(worker);
        if (node != nullptr) {
            ReportError(worker, identToken.site, "Redefinition of identifier: " + identToken.string);
            return nullptr;
        } else {
            auto var = CreateVariable(&worker->arena, worker->lex.token.site, identToken.string.c_str(), expr, 0);
            AssignIdent(worker->currentBlock, var, identToken.string);
            return var;
        }
	} break;

	case TOKEN_TYPE_DEFINE: {
		worker->lex.nextToken(); // Eat the typedef
		if (worker->lex.token.type == TOKEN_PAREN_OPEN) {
			auto functionSet = static_cast<ASTFunctionSet*>(FindNodeWithIdent(worker->currentBlock, identToken.string));
			return ParseFunction(worker, functionSet, identToken);	// Parse function will create the appropriate function set
        } else if (worker->lex.token.type == TOKEN_STRUCT) {
			return ParseStructDefn(worker, identToken, node);
		} else {
			ReportError(worker, worker->lex.token.site, "Could not define type with identifier: " + identToken.string +" (unknown keyword '" + worker->lex.token.string + "')");
			worker->lex.nextToken();
			return nullptr;
		}
	} break;

	case TOKEN_PAREN_OPEN:	{
		return ParseCall(worker, identToken);
	} break;

	case TOKEN_ACCESS: {
		auto structVariable = static_cast<ASTVariable*>(node);
		assert(structVariable->nodeType == AST_VARIABLE);
		return ParseMemberOperation(worker, structVariable);
	} break;

	case TOKEN_EQUALS:
	case TOKEN_ADD_EQUALS:
	case TOKEN_SUB_EQUALS:
	case TOKEN_MUL_EQUALS:
	case TOKEN_DIV_EQUALS:
		Operation operation;
		switch (worker->lex.token.type) {
		case TOKEN_EQUALS:		operation = OPERATION_ASSIGN; break;
		case TOKEN_ADD_EQUALS:	operation = OPERATION_ADD;		break;
		case TOKEN_SUB_EQUALS:	operation = OPERATION_SUB;		break;
		case TOKEN_MUL_EQUALS:	operation = OPERATION_MUL;		break;
		case TOKEN_DIV_EQUALS:	operation = OPERATION_DIV;		break;
		default: ReportError(worker, worker->lex.token.site, "Unkown operator: " + worker->lex.token.string);
		} worker->lex.nextToken();	// Eat the operator

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

	assert(false && "Internal Error: Reached End of ParseIdentifier without handling the token");
	return nullptr;
}

static inline ASTNode* ParseIF(Worker* worker) {
	worker->lex.nextToken();	// Eat the IF lex.token
	auto expr = ParseExpr(worker);
	if (!expr) {
		ReportError(worker, worker->lex.token.site, "Could not parse expresion for IF statement evaluation");
	}

	auto ifStatement = CreateIfStatement(&worker->arena, expr);
	auto body = ParseStatement(worker);
	ifStatement->ifBody = body;

	if (worker->lex.token.type == TOKEN_ELSE) {
		worker->lex.nextToken(); //Eat the else keyword!
		auto elseBody = ParseStatement(worker);
		ifStatement->elseBody = elseBody;
	}
	return ifStatement;
}

static inline ASTNode* ParseIter (Worker* worker, const std::string& identName) {
	LOG_VERBOSE(worker->lex.token.site << "Parsing a iter statement");
	worker->lex.nextToken(); // Eat the iter

	auto expr = ParseExpr(worker);
	if (!expr) LOG_ERROR(worker->lex.token.site << "Could not parse expression to the right of iter");

	if (worker->lex.token.type == TOKEN_TO) {
		 worker->lex.nextToken(); 	// Eat the to
		 if (identName != "") {
			auto block = CreateBlock(&worker->arena, worker->currentBlock);
			auto var = CreateVariable(&worker->arena, worker->lex.token.site, identName.c_str());
			var->type = global_S32Type;	// HACK
			var->initalExpression = expr;
			AssignIdent(block, var, identName);

			auto endExpr = ParseExpr(worker);
			if (!endExpr) LOG_ERROR(worker->lex.token.site << "Could not parse Expression after TO keyword");
			ParseBlock(worker, block);
			auto iter = CreateIter(&worker->arena, var, expr, endExpr, nullptr, block);
			return iter;

		} else {
			ReportError(worker, worker->lex.token.site, "iter statements that iterrate through a range must be declared with an identifier to hold the index!");
			ParseExpr(worker);	//eat the other expr
			//TODO skip block or somthing?
			return nullptr;
		}
	}

	else if (worker->lex.token.type == TOKEN_BLOCK_OPEN) {
		auto block = ParseBlock(worker);
		LOG_ERROR("Not dealing with these for now");
	}
	return nullptr;
}

static inline ASTNode* ParseBlock (Worker* worker, ASTBlock* block) {
	assert(worker->lex.token.type == TOKEN_BLOCK_OPEN);
	worker->lex.nextToken();	 // Eat the SCOPE_OPEN
	auto previousScope = worker->currentBlock;
	if (block == nullptr) {
		assert(false && "I dont think that this should ever happen!");
		block = CreateBlock(&worker->arena, previousScope);
	}

	worker->currentBlock = block;
	while (worker->lex.token.type != TOKEN_BLOCK_CLOSE && worker->lex.token.type != TOKEN_END_OF_BUFFER) {
		auto node = ParseStatement(worker);
		block->members.push_back(node);
	}

    auto& lex = worker->lex;

    worker->currentBlock = previousScope;
    if (lex.token.type == TOKEN_END_OF_BUFFER) {
        return block;
    }

	worker->lex.nextToken();	// Eat the close scope

	return block;
}

static char* ReadFileIntoMemory(const char* filename) {
	char* result = nullptr;
	FILE* file = fopen(filename, "r");

	if (file) {
		fseek(file, 0, SEEK_END);
		size_t filesize = ftell(file);
		fseek(file, 0, SEEK_SET);

		result = static_cast<char*>(malloc(filesize + 1));
		fread(result, filesize, 1, file);
		result[filesize] = 0;

		fclose(file);
	}

	return result;
}


int ParseFile(Worker* worker, const std::string& rootDir, const std::string& filename) {
    auto fileBuffer = ReadFileIntoMemory(filename.c_str());
	if (fileBuffer == nullptr) {
		LOG_ERROR("Could not open filename: " << filename);
		return -1;
	}

	LOG_INFO("Parsing " << filename);
    worker->lex.SetBuffer(fileBuffer);
    worker->lex.token.site.filename = filename;
    worker->lex.nextToken();
	while (worker->lex.token.type != TOKEN_END_OF_BUFFER) {
		ParseStatement(worker);
	}

    free(fileBuffer);
	return 0;
}
