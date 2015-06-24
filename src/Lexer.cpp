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
	this->nextChar = ' ';
}

Lexer::~Lexer() {

}

void Lexer::AppendNext() {
	lastChar = stream->get();
	nextChar = stream->peek();
	colNumber++;
	if (lastChar == '\n') {
		lineNumber++;
		colNumber = 1;
	} else if (lastChar != ' ') {
		tokenString += lastChar;
	}
}

void Lexer::EatNext() {
	lastChar = stream->get();
	nextChar = stream->peek();

	if (lastChar == '\n') {
		lineNumber++;
		colNumber = 1;
	}
}

void Lexer::EatWhitespaces() {
	while(isspace(nextChar))
		EatNext();
}


Token Lexer::GetToken() {
	tokenString = ""; //Reset the tokenstring
	EatWhitespaces();
	assert(nextChar != ' ' && nextChar != '\n' && nextChar != '\r');

	//We now have some non-whitespace character to lex.
	//At this point the tokenString is empty and the nextChar is the character that is a non whitespace
	//Last char should be a whitespace or somthing that was tokenized seperatly like an opperator
	//Save the currentLine and column number so we known where the token originated
	currentTokenLineNumber = lineNumber;
	currentTokenColumnNumber = colNumber;

	//KEYWORD, IDENTIFIER, TYPE
	//If the character begins with a letter it must be a keyword, type, or identifier
	if(isalnum(nextChar)) {
		while (isalnum(nextChar)) {
			AppendNext();
		}
		//The nextChar is now somthing that is not alphanum so the end of our token is reached
		//Check to see if this token is a keyword
		//If its not any keywords then it must be a identifier
		if (tokenString == "func") return Token::Func;
		if (tokenString == "foreign") return Token::Foreign;
		if (tokenString == "if") return Token::If;
		if (tokenString == "else") return Token::Else;
		return Token::Identifer;
	}

	//NUMERIC LITERAL
	if (isdigit(nextChar)) { //Character was not alpha so we already know that it will not be an identifier
		while(isdigit(nextChar)) {
			AppendNext();
		}	//TODO dont worry about FP for now
		return Token::Number;
	}

	//COMMENTS
	if (nextChar == '/') {
		EatNext();	//Eat the '/ 'char
		// Check to see if the next char is another '/' If so this token is a comment
		if (nextChar == '/') {
			EatNext(); //Eats the second '/'
			while(nextChar != EOF && nextChar != '\n' && nextChar != '\r') {
				EatNext();	//Now we eat the comment body itsself
			}
			//We have reached the end of the comment.  If is not the end of the file get the next token
			if(nextChar != EOF) {
				return GetToken();
			}
		}
	}

	//COLON TOKENS
	if (nextChar == ':') {
		AppendNext();
		if(nextChar == ':') {
			AppendNext();
			return Token::TypeDefine;
		}
		else if (nextChar == '='){
			AppendNext();
			return Token::TypeInfer;
		}
		else {
			return Token::TypeAssign;
		}
	}

	//BRACES, BRACKETS, SUBSCRIPTS
	else if (nextChar == '(') {
		AppendNext();
		return Token::ParenOpen;
	}
	else if (nextChar == ')') {
		AppendNext();
		return Token::ParenClose;
	}
	else if (nextChar == '{') {
		AppendNext();
		return Token::ScopeOpen;
	}
	else if (nextChar == '}') {
		AppendNext();
		return Token::ScopeClose;
	}
	else if (nextChar == EOF) {
		//Dont append or ead the EOF
		return Token::EndOfFile;
	}
	else{
		AppendNext();
		return Token::Unkown;
	}
}

//This function will be called after a type opperator has been seen by the parser
//Returns the type that is represented by the tokenString
Type Lexer::GetType() {
	tokenString = "";
	EatWhitespaces();

	//All types must start with a alpha char
	if (isalpha(nextChar)) {
		while(isalnum(nextChar)) {
			AppendNext();
		}
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
