#ifndef LEXER_HPP
#define LEXER_HPP

#include <iostream>
#include <istream>
#include <fstream>

#include "Common.hpp"

enum class Token {
	Unkown,

	Identifier,

	TypeDefine,
	TypeDeclare,
	TypeInfer,
	TypeReturn,

	//BinOps
	EQUALS,
	ADD,
	SUB,
	MUL,
	DIV,
	MOD,
	ADD_EQUALS,
	SUB_EQUALS,
	MUL_EQUALS,
	DIV_EQUALS,
	MOD_EQUALS,

	//Control flow
	IF,
	ELSE,
	RETURN,

	Foreign,
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
		output << "[" << position.filename << "::" << position.lineNumber << ":" << position.columNumber<< "]";
		return output;
	}
};


class Lexer {
public:
	std::string tokenString;
	Token token;


	FilePosition filePos;
	Lexer(std::string filename, std::ifstream* stream);
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

#endif //LEXER_HPP
