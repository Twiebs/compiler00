#ifndef LLVMLANG_PARSERCOMMON_HPP
#define LLVMLANG_PARSERCOMMON_HPP
#if 0

#include "Lexer.hpp"
#include "AST.hpp"

ASTExpression* ParseExpr(MemoryArena* arena, Lexer* lex);


inline bool IsUnaryOperator(TokenType token) {
	switch (token) {
	case TOKEN_ADDRESS:
	case TOKEN_VALUE:
	case TOKEN_LOGIC_NOT:
		return true;
	default:
		return false;
	}
}

inline bool IsUnaryOperator(const Token& token) {
	return IsUnaryOperator(token.type);
}
#endif

#endif //LLVMLANG_PARSERCOMMON_HPP
