#pragma once

#include <iostream>
#include <istream>
#include <fstream>

#include "Common.hpp"

enum class TokenType {
	UNKOWN,
	IMPORT,
	FOREIGN,
	IDENTIFIER,

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
	STRING,
	STRING_OPEN,
	STRING_CLOSE,

	END_OF_FILE
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
struct LexState {
	std::ifstream stream;
	char lastChar, nextChar;
	U32 lineNumber, colNumber;
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

//Do somthing like this instead!
//Tokens can now be explicityl placed and then initalized by the lexing functions
//TODO Nest a enum in here?
//Is that somthing that works?
struct Token {
	TokenType type;
	FileSite site;
	std::string string;


};

//Consider renaming LexNextToken(lex);
void LexToken(LexState& state, Token& token);
void NextToken(LexState& state);
void ExpectAndEat(LexState& state);


#if 0
class Lexer {
public:
	std::string tokenString;
	Token token;
	FilePosition filePos;

	Lexer(std::string filename);
	~Lexer();


private:
	int lineNumber = 0;
	void NextToken();
	int colNumber = 1;
	std::ifstream stream;

	char lastChar;
	char nextChar;

	Token GetToken();


	void AppendNext();
	void EatNext();
	void EatWhitespaces();
};
#endif
