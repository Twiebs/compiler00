#ifndef LLVMLANG_PARSERCOMMON_HPP
#define LLVMLANG_PARSERCOMMON_HPP

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

//
//class Parser {
//	MemoryArena* arena;
//	Lexer* lex;
//	ASTBlock* currentBlock;
//
//	void ReportError(const FileSite& site, const char* fmt, ...);
//
//	ASTCast* ParseCast(const Token& identToken);
//	ASTCall* ParseCall(const Token& identToken);
//
//	void ParseMemberAccess(ASTVariable* structVar, ASTMemberAccess* memberAccess);
//
//public:
//	ASTExpression* ParseExpr();
//};

#endif //LLVMLANG_PARSERCOMMON_HPP
