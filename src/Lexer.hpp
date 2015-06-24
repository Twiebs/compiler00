#ifndef LEXER_HPP
#define LEXER_HPP

#include <iostream>
#include <istream>
#include <fstream>

#include "Common.hpp"

enum class Type {
	Undefined,
	Int32,
	Int64,
	Float32,
	Float64,
};

enum class Token {
	Unkown,

	Identifer,
	TypeDefine,
	TypeAssign,
	TypeInfer,

	Func,
	Foreign,
	Number,
	AssignmentOpperator,

	If,
	Else,

	ParenOpen,
	ParenClose,
	ScopeOpen,
	ScopeClose,

	EndOfFile
};

class Lexer {
public:
	std::string tokenString;
	int lineNumber = 0;
	int colNumber = 1;

	uint32 currentTokenLineNumber;
	uint32 currentTokenColumnNumber;


	Lexer(std::ifstream* stream);
	~Lexer();

	Token GetToken();
	Type GetType();

private:
	std::ifstream* stream;



	char lastChar;
	void NextChar();
	char PeekChar();

	void EatWhitespaces();

};

#endif //LEXER_HPP
