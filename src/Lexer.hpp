#pragma once

#include <iostream>
#include <istream>
#include <fstream>

#include "Common.hpp"

enum class Token {
	UNKOWN,
	IMPORT,
	FOREGIN,

	Identifier,

	TypeDefine,
	TypeDeclare,
	TypeInfer,
	TypeReturn,

	ADD,
	SUB,
	MUL,
	DIV,
	MOD,
	EQUALS,
	ADD_EQUALS,
	SUB_EQUALS,
	MUL_EQUALS,
	DIV_EQUALS,
	MOD_EQUALS,

	IF,
	ELSE,
	FOR,
	WHILE,
	RETURN,


	Number,

	ParenOpen,
	ParenClose,
	ScopeOpen,
	ScopeClose,

	EndOfFile
};

// Represents a site in a file with a lineNumber and a columNumber
// Can be printed through standard output.  The lexer is resposible for this bookeeping
// And ASTNodes will copy the value of the lexers fileposition when they are incepted
struct FilePosition {
	std::string filename;
	uint32 lineNumber;
	uint32 columNumber;
	friend std::ostream& operator<<(std::ostream& output, const FilePosition& position) {
		output << "[" << position.filename << " " << position.lineNumber << ":" << position.columNumber<< "]";
		return output;
	}
};


class Lexer {
public:
	std::string tokenString;
	Token token;
	FilePosition filePos;


	Lexer(std::string filename);
	~Lexer();

	void NextToken();


private:
	int lineNumber = 0;
	int colNumber = 1;
	std::ifstream* stream;

	char lastChar;
	char nextChar;

	Token GetToken();


	void AppendNext();
	void EatNext();
	void EatWhitespaces();
};
