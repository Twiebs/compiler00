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
	TypeReturn,

	Plus,
	Minus,

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
