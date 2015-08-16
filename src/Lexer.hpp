#pragma once

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

	TOKEN_ParenOpen,
	TOKEN_ParenClose,
	TOKEN_ScopeOpen,
	TOKEN_ScopeClose,
	TOKEN_STRING,
	TOKEN_STRING_OPEN,
	TOKEN_STRING_CLOSE,

	TOKEN_END_OF_FILE
};

// Represents a site in a file with a lineNumber and a columNumber
// Can be printed through standard output.  The lexer is resposible for this bookeeping
// And ASTNodes will copy the value of the lexers fileposition when they are incepted
//TODO should we save the file position where a token came from inside a token struct
//Instead of passing around the token enum

// Represents a position in the file where the lexer has visitied
//Change to fileSite it makes more sense IMO?
//TODO change FilePosition to FileSite
//The we can do site.lineNumber;
//I like that a lot better

//This is the minum information that the lexer needs to do its job properly
//For now atleast...
//TODO storing a lastChar is probably compleatly irrelevant
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
	std::ifstream stream;
	char lastChar = 0, nextChar = 0;
	U32 lineNumber = 0, colNumber = 0;
	Token token;

	void next();

private:
	void eatNextChar();
	void appendNextChar();
};
