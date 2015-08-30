#pragma once

#define USE_SCOPE_INDENT
#define SCOPE_SPACE_COUNT 2

#include <iostream>
#include <istream>
#include <fstream>

#include "Common.hpp"

enum TokenType {
	TOKEN_UNKOWN,
	TOKEN_IMPORT,
	TOKEN_FOREIGN,
	TOKEN_IDENTIFIER,

	TOKEN_TYPE_DEFINE,
	TOKEN_TYPE_DECLARE,
	TOKEN_TYPE_INFER,
	TOKEN_TYPE_RETURN,

	TOKEN_STRUCT,
	TOKEN_ACCESS,

	TOKEN_POINTER,
	TOKEN_DEREF,

	TOKEN_ADD,
	TOKEN_SUB,
	TOKEN_MUL,
	TOKEN_DIV,
	TOKEN_MOD,
	TOKEN_EQUALS,
	TOKEN_ADD_EQUALS,
	TOKEN_SUB_EQUALS,
	TOKEN_MUL_EQUALS,
	TOKEN_DIV_EQUALS,
	TOKEN_MOD_EQUALS,

	TOKEN_IF,
	TOKEN_ELSE,
	TOKEN_ITER,
	TOKEN_TO,
	TOKEN_RETURN,

	TOKEN_NUMBER,

	TOKEN_PAREN_OPEN,
	TOKEN_PAREN_CLOSE,
	TOKEN_SCOPE_OPEN,
	TOKEN_SCOPE_CLOSE,
	TOKEN_STRING,
	TOKEN_STRING_CLOSE,

	TOKEN_STRING_OPEN,
	TOKEN_EOF
};

struct FileSite {
	std::string filename;
	U32 lineNumber;
	U32 columNumber;
	friend std::ostream& operator<<(std::ostream& output, const FileSite& site) {
		output << "[" << site.filename << " " << site.lineNumber << ":" << site.columNumber<< "]";
		return output;
	}
};

struct Token {
	FileSite site;
	TokenType type;
	std::string string;
};

class Lexer {
public:
	Token token;
	std::ifstream stream;
	char lastChar = 0, nextChar = 0;
	U32 lineNumber = 0, colNumber = 0;

#ifdef USE_SCOPE_INDENT
	int currentIndentLevel;
#endif

	void next(bool statement = false);

private:

	void eatNextChar();
	void appendNextChar();
	int GetIndentLevel();
};
