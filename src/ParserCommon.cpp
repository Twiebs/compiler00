#include "ParserCommon.hpp"

#include <functional>

static UnaryOperator TokenToUnaryOp(const Token& token) {
	switch (token.type) {
	case TOKEN_LOGIC_NOT: return UNARY_NOT;
	}
	
	assert(false);
	return static_cast<UnaryOperator>(0);
}



ASTExpression* ParseExpr(MemoryArena* arena, Lexer* lex) {
	auto parsePrimary = [](MemoryArena* arena, Lexer* lex) -> ASTExpression* {
		switch (lex->token.type) {
		case TOKEN_TRUE:
		{
			lex->nextToken();
			return CreateIntegerLiteral(arena, 1);
		}
		case TOKEN_FALSE:
		{
			lex->nextToken();
			return CreateIntegerLiteral(arena, 0);
		}
		case TOKEN_STRING:
		{
			auto str = CreateStringLiteral (arena, lex->token.string);
			lex->nextToken();
			return str;
		}

		case TOKEN_NUMBER:
		{
			LOG_VERBOSE("Parsing a numberExpression!");
			auto dotPos = lex->token.string.find(".");
			bool isFloat = dotPos == std::string::npos ? false : true;
			if (isFloat) {
				if (lex->token.string.substr(dotPos + 1).find(".") != std::string::npos) {
					// ReportError(worker, worker->token.site, "Floating Point value contains two decimal points!");
				}
				auto value = std::stof(lex->token.string);
				auto result = CreateFloatLiteral(arena, value);
				lex->nextToken();
				return result;
			} else {
				auto value = std::stoi(lex->token.string);
				auto result = CreateIntegerLiteral(arena, value);
				lex->nextToken();
				return result;
			}
		}

		}
		return nullptr;
	};

	std::function<ASTExpression*(MemoryArena*, Lexer*, ASTExpression*, int)> parseBinary
		= [&parsePrimary, &parseBinary](MemoryArena* arena, Lexer* lex, ASTExpression* lhs, int expressionPrec) -> ASTExpression* {
		assert(lhs != nullptr);
		while (true) {

			auto tokenPrec = GetTokenPrecedence(lex->token);
			if (tokenPrec < 0) return lhs;	// Bail if there is no binop

			auto binopToken = lex->token;
			lex->nextToken();

			// We have a binop lets see what is on the other side!
			ASTExpression* rhs = parsePrimary(arena, lex);
			if (rhs == nullptr) {
				//ReportError(worker, binopToken.site, "Could not parse primary expression to the right of binary opperator '" + binopToken.string	+ "'");
				return nullptr;
			}

			auto nextPrec = GetTokenPrecedence (lex->token);
			if (tokenPrec < nextPrec) {
				rhs = parseBinary(arena, lex, rhs, tokenPrec + 1);
				if (rhs == nullptr) {
					LOG_ERROR("Could not parse recursive rhsParsing!");
					return nullptr;
				}
			}

			lhs = (ASTExpression*)CreateBinaryOperation(arena, TokenToOperation(binopToken), lhs, rhs);
		}	 // Goes back to the while loop
	};


	auto lhs = parsePrimary(arena, lex);
	if (lhs == nullptr) return nullptr;
	return parseBinary(arena, lex, lhs, 0);
}




//void Parser::ParseMemberAccess (ASTVariable* structVar, ASTMemberAccess* memberAccess) {
//	auto structType = (ASTStruct*)structVar->type;
//	if (structType == nullptr) {
//		std::vector<char*> memberNames;
//		while (lex->token.type == TOKEN_ACCESS) {
//			lex->nextToken();
//			if (lex->token.type != TOKEN_IDENTIFIER)
//				ReportError(lex->token.site, "A struct member access must reference an identifier");
//			memberNames.push_back((char*)Allocate(arena, lex->token.string.length() + 1));
//			lex->nextToken();
//		}
//		memberAccess->memberCount = memberNames.size();
//		memberAccess->memberNames = (char**)Allocate(arena, memberNames.size() * sizeof(char*));
//		memberAccess->indices = (U32*)Allocate(arena, memberNames.size() * sizeof(U32));
//		memcpy(memberAccess->memberNames, &memberNames.front(), memberNames.size() * sizeof(char*));
//	} else {
//
//		std::vector<U32> indices;
//		auto currentStruct = structType;
//		while (lex->token.type == TOKEN_ACCESS) {
//			lex->nextToken(); // eat the member access
//			auto memberIndex = GetMemberIndex(currentStruct, lex->token.string);
//			if (memberIndex == -1) {
//				// ReportError(lex->token.site, lex->token.string + " does not name a member in struct '" + currentStruct->name + "'");
//			} else {
//				auto memberType = currentStruct->members[memberIndex].type;
//				if (memberType->nodeType == AST_STRUCT)
//					currentStruct = (ASTStruct*)memberType;
//			}
//			indices.push_back(memberIndex);
//			lex->nextToken();	// eat the member ident
//		}
//		memberAccess->memberCount = indices.size();
//		memberAccess->indices = (U32*)Allocate(arena, indices.size() * sizeof(U32));
//		memberAccess->memberNames = nullptr;
//		memcpy(memberAccess->indices, &indices.front(), indices.size() * sizeof(U32));
//	}
//};
//
//
//ASTCall* Parser::ParseCall(const Token& identToken) {
//	if (currentBlock->parent == nullptr) {
//		ReportError(lex->token.site, "Can not call functions outside a block!  Did you mean to use :: ?");
//	}
//
//	std::vector<ASTExpression*> args;
//	lex->nextToken(); // Eat the open paren
//	while (lex->token.type != TOKEN_PAREN_CLOSE) {
//		ASTExpression* expr = ParseExpr();
//		if (expr == nullptr) {
//			ReportError(lex->token.site, "Could not resolve expression for argument at index %d in call to function named: %s", args.size(), identToken.string.c_str());
//		} else {
//			args.push_back(expr);
//		}
//	}
//
//	lex->nextToken();		// Eat the close paren
//
//	ASTCall* call = CreateCall(arena, &args[0], args.size(), identToken.string);
//	return call;
//}
//
//ASTCast* Parser::ParseCast (const Token& identToken) {
//	lex->nextToken();
//	auto typeDefn = (ASTDefinition*)FindNodeWithIdent(currentBlock, identToken.string);
//	assert(typeDefn->nodeType == AST_DEFINITION);
//	auto expr = ParseExpr();
//
//	if (lex->token.type != TOKEN_PAREN_CLOSE) {
//		ReportError(lex->token.site, "expected close parren enlosing expression when casting to type %s", identToken.string.c_str());
//		return nullptr;
//	} else {
//		lex->nextToken();
//		auto cast = CreateCast(arena, typeDefn, expr);
//		return cast;
//	}
//}


ASTUnaryOp* ParseUnaryOperation(MemoryArena* arena, Lexer* lex) {
	assert(IsUnaryOperator(lex->token));
	lex->nextToken();

	auto expr = ParseExpr(arena, lex);
	auto op = TokenToUnaryOp(lex->token);
	auto unaryOperation = arena->alloc<ASTUnaryOp>(op, expr);
	return unaryOperation;
}



//ASTExpression* Parser::ParseExpr() {
//
//    auto parsePrimary = [&](MemoryArena* arena, Lexer* lex) -> ASTExpression* {
//	switch(lex->token.type) {
//
//	case TOKEN_TRUE:
//		lex->nextToken();
//		return global_trueLiteral;
//	case TOKEN_FALSE:
//		lex->nextToken();
//		return global_falseLiteral;
//
//	case TOKEN_ADDRESS:
//	case TOKEN_VALUE:
//	case TOKEN_LOGIC_NOT:
//		return ParseUnaryOperation(arena, lex);
//
//	case TOKEN_IDENTIFIER:
//	{
//		Token identToken = lex->token;
//		lex->nextToken();
//
//		// TODO more robust error checking when receving statement tokens in an expression
//		ASTNode* node;
//		switch (lex->token.type) {
//		case TOKEN_TYPE_DECLARE:
//		case TOKEN_TYPE_DEFINE:
//		case TOKEN_TYPE_INFER:
//		case TOKEN_TYPE_RETURN:
//			// ReportError(worker, lex->token.site, "Unexpected token when parsing expression.	Token '" + lex->token.string + "' cannot be used in an expression");
//			lex->nextToken();	// eat whatever that token was... hopefuly this will ha
//			break;
//		default:
//			node = FindNodeWithIdent(currentBlock, identToken.string);
//			if (node == nullptr) {
//				ReportError(identToken.site, "There is no expression matching the identifier (%s)", identToken.string.c_str());
//				lex->nextToken();
//				return nullptr;
//			}
//			break;
//		}
//
//		if (lex->token.type == TOKEN_PAREN_OPEN) {
//			if (node->nodeType == AST_DEFINITION || node->nodeType == AST_STRUCT) {
//				auto cast = ParseCast(identToken);
//				return (ASTExpression*)cast;
//			} else {
//				auto call = ParseCall(identToken);
//				return (ASTExpression*)call;
//			}
//		} else if (lex->token.type == TOKEN_ACCESS) {
//			auto structVar = (ASTVariable*)node;
//			auto structDefn = (ASTStruct*)structVar->type;
//			auto expr = CreateMemberExpr(arena, structVar, unary);
//			ParseMemberAccess(structVar, &expr->access);
//			return expr;
//
//			//			if (structDefn == nullptr) {
//			//				while (lex->token.type == TOKEN_ACCESS) {
//			//					lex->nextToken();
//			//					if (lex->token.type != TOKEN_IDENTIFIER)
//			//						ReportError(worker, &lex->token.site, "A struct member access must reference an identifier");
//			//					memberNames.push_back(lex->token.string);
//			//					lex->nextToken();
//			//				}
//			//
//			//				//auto expr = CreateMemberExpr(&worker->arena, structVar, unary, &indices[0], indices.size());
//			//				auto expr = CreateMemberExpr(&worker->arena, structVar, unary, memberNames);
//			//				return expr;
//			////			} else {
//			//				if (structDefn->nodeType != AST_STRUCT) ReportError(worker, lex->token.site, identToken.string + " does not name a struct type!  It is a " + ToString(node->nodeType));
//			//				auto currentStruct = structDefn;
//			//				ASTDefinition* exprType = nullptr;
//			//				std::vector<U32> indices;
//			//				while(lex->token.type == TOKEN_ACCESS) {
//			//					lex->nextToken(); // eat the member access
//			//					auto memberIndex = GetMemberIndex(currentStruct, lex->token.string);
//			//					if (memberIndex == -1) {
//			//						ReportError(worker, lex->token.site, lex->token.string + " does not name a member in struct '" + currentStruct->name + "'");
//			//					} else {
//			//						indices.push_back(memberIndex);
//			//						auto memberType = currentStruct->members[memberIndex].type;
//			//						if(memberType->nodeType == AST_STRUCT)
//			//							currentStruct = (ASTStruct*)memberType;
//			//						exprType = memberType;
//			//					}
//			//					lex->nextToken();	// eat the member ident
//			//				}
//			//
//			//				auto expr = CreateMemberExpr(&worker->arena, structVar, unary, memberNames);
//			//				return expr;
//			//			}
//
//
//
//
//		} else {
//			auto var = (ASTVariable*)node;
//			auto expr = CreateVarExpr(&worker->arena, var, unary);
//			return expr;
//		}
//		LOG_ERROR("SOMTHING TERRIBLE HAS HAPPENED!");
//	} break;
//
//	case TOKEN_PAREN_OPEN:
//	{
//		lex->nextToken(); // Eat the open paren
//		auto expr = ParseExpr(worker);
//		if (lex->token.type != TOKEN_PAREN_CLOSE) {
//			ReportError(worker, lex->token.site, "Expected close paren in expression");
//		}
//		lex->nextToken(); // Eat the close paren
//		return expr;
//	} break;
//
//	case TOKEN_NUMBER:
//	{
//		LOG_VERBOSE("Parsing a numberExpression!");
//		auto dotPos = lex->token.string.find(".");
//		bool isFloat = dotPos == std::string::npos ? false : true;
//		if (isFloat) {
//			if (lex->token.string.substr(dotPos + 1).find(".") != std::string::npos) {
//				ReportError(worker, lex->token.site, "Floating Point value contains two decimal points!");
//			}
//			auto value = std::stof(lex->token.string);
//			auto result = CreateFloatLiteral(&worker->arena, value);
//			lex->nextToken(); // Eat the float literal
//			return result;
//		} else {
//			auto value = std::stoi(lex->token.string);
//			auto result = CreateIntegerLiteral(&worker->arena, value);
//			lex->nextToken(); // Eat the int literal
//			return result;
//		}
//	}
//
//	case TOKEN_STRING:
//	{
//		auto str = CreateStringLiteral (&worker->arena, lex->token.string);
//		lex->nextToken(); // Eat the string token
//		return str;
//	}
//
//	default:
//		ReportError(worker, &lex->token.site, "Error when expecting expression: Unknown Token(%s)", lex->token.string.c_str());
//		lex->nextToken();
//		return nullptr;
//	}
//		// This is dead code it will never happen.
//    };
//
//    std::function<ASTExpression*(MemoryArena*, Lexer*, ASTExpression*, int)> parseBinary
//            = [&parsePrimary, &parseBinary](MemoryArena* arena, Lexer* lex, ASTExpression* lhs, int expressionPrec) -> ASTExpression* {
//        assert(lhs != nullptr);
//        while (true) {
//
//            auto tokenPrec = GetTokenPrecedence(lex->token);
//            if (tokenPrec < 0) return lhs;	// Bail if there is no binop
//
//            auto binopToken = lex->token;
//            lex->nextToken();
//
//            // We have a binop lets see what is on the other side!
//            ASTExpression* rhs = parsePrimary(arena, lex);
//            if (rhs == nullptr) {
//                //ReportError(worker, binopToken.site, "Could not parse primary expression to the right of binary opperator '" + binopToken.string	+ "'");
//                return nullptr;
//            }
//
//            auto nextPrec = GetTokenPrecedence (lex->token);
//            if (tokenPrec < nextPrec) {
//                rhs = parseBinary(arena, lex, rhs, tokenPrec + 1);
//                if (rhs == nullptr) {
//                    LOG_ERROR("Could not parse recursive rhsParsing!");
//                    return nullptr;
//                }
//            }
//
//            lhs = (ASTExpression*)CreateBinaryOperation(arena, TokenToOperation(binopToken), lhs, rhs);
//        }	 // Goes back to the while loop
//    };
//
//
//    auto lhs = parsePrimary(arena, lex);
//    if (lhs == nullptr) return nullptr;
//    return parseBinary(arena, lex, lhs, 0);
//}