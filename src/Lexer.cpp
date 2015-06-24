/*
 * Lexer.cpp
 *
 *  Created on: Jun 18, 2015
 *      Author: torin
 */

#include "Lexer.hpp"

Lexer::Lexer(std::ifstream* stream) {
	this->stream = stream;
	this->lastChar = ' ';
}

Lexer::~Lexer() {

}

char Lexer::PeekChar() {
	return stream->peek();
}

void Lexer::NextChar() {
	lastChar = stream->get();
	colNumber++;
	if (lastChar == '\n') {
		lineNumber++;
		colNumber = 1;
	} else if (lastChar != ' ') {
		tokenString += lastChar;
	}
}

void Lexer::EatWhitespaces() {
	while(isspace(lastChar))
		NextChar();
}


Token Lexer::GetToken() {
	tokenString = ""; //Reset the tokenstring
	EatWhitespaces();

	//We now have somthing to lex
	//Save the currentLine and colum number so we known where the token originiated
	currentTokenLineNumber = lineNumber;
	currentTokenColumnNumber = colNumber;

	//KEYWORD, IDENTIFIER, TYPE
	//If the character begins with a letter it must be a keyword, type, or identifier
	if (isalpha(lastChar)) {
		while (isalnum(lastChar))
			NextChar();	//Grab all the alnum chars in the identifier
		//Check to see if this token is a keyword
		if (tokenString == "func") return Token::Func;
		if (tokenString == "foreign") return Token::Foreign;
		if (tokenString == "if") return Token::If;
		if (tokenString == "else") return Token::Else;
		//If its not any keywords then it must be a identifier
		return Token::Identifer;
	}

	//NUMERIC LITERAL
	//If the character beings with a digit then we are lexing a
	//Numerical literal
	if (isdigit(lastChar)) { //Charcter was not alpha so we already know that it will not be an identifier
		do NextChar();
		while (isdigit(lastChar));
		return Token::Number;
	}

	//COMMENTS
	//Checks if this token is a comment
	//Will Eat the line and recusivly get a token if it is
	if (lastChar == '/') {
		NextChar();	//Eat the '/ 'char
		// Check to see if the next char is another /
		// If so this token is a comment
		if (lastChar == '/') {
			//Eat all the chars until a newline or the EOF is reached
			do NextChar();
			while (lastChar != EOF && lastChar != '\n' && lastChar != '\r');
			if (lastChar != EOF)
				return GetToken();
		}
	}

	//SYMBOL TOKENS
	Token token;

	//COLON TOKENS
	//TypeDefine, TypeInfer, and TypeAssign
	if (lastChar == ':') {
		char nextChar = PeekChar();
		//If a colon is seen check what comes next
		//If its another colon then the token is a typeDefine
		if(nextChar == ':'){
			NextChar();	//Eat the first ':'
			token = Token::TypeDefine;
		}
		//If it a = then the token is a type inference
		else if(nextChar == '=') {
			NextChar(); //Eat the ':'
			token = Token::TypeInfer;
		}
		//Otherwise its just a plain old colon and its a type assignment
		else
			token = Token::TypeAssign;
	}
	//The token was not a colon
	//Check other token symbols
	else if (lastChar == '(') token = Token::ParenOpen;
	else if (lastChar == ')') token = Token::ParenClose;
	else if (lastChar == '{') token = Token::ScopeOpen;
	else if (lastChar == '}') token = Token::ScopeClose;
	else if (lastChar == EOF) token = Token::EndOfFile;
	else token = Token::Unkown;

	NextChar(); //Eat the symbol token
	return token;	//Return it
}

//This function will be called after a type opperator has been seen by the parser
//Returns the type that is represented by the tokenString
Type Lexer::GetType() {
	tokenString = "";
	EatWhitespaces();

	//All types must start with a alpha char
	if (isalpha(lastChar)) {
		do NextChar();
		while (isalnum(lastChar));
	//Get the tokenString while there are still alphaNumeric charcters

	//Check to see what type the string represents
		if (tokenString == "Int32") 	 return Type::Int32;
		if (tokenString == "Int64") 	 return Type::Int64;
		if (tokenString == "Float32") return Type::Float32;
		if (tokenString == "Float64") return Type::Float64;
		//If its not anyone of these then the type is undefined
		return Type::Undefined;
	}
	//The charcter did not start with a letter
	//Types cannot start with a number so this is not a valid type
	return Type::Undefined;
}
