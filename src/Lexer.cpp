/*
 * Lexer.cpp
 *
 *  Created on: Jun 18, 2015
 *      Author: torin
 */

#include "Lexer.hpp"

Lexer::Lexer(std::string filename) {
	this->filePos.filename = filename;

	stream = new std::ifstream(filename.c_str());
	if(!stream->is_open()) {
		std::cout << "Error: Can not open file: " << filename << "\n";
		abort();
	}

	this->nextChar = stream->peek();
	this->lastChar = stream->get();
}

Lexer::~Lexer() {

}

void Lexer::AppendNext() {
	lastChar = nextChar;
	nextChar = stream->get();
	colNumber++;
	if (lastChar == '\n') {
		lineNumber++;
		colNumber = 1;
	} else if (lastChar != ' ') {
		tokenString += lastChar;
	}
}

void Lexer::EatNext() {
	lastChar = nextChar;
	nextChar = stream->get();

	if (lastChar == '\n') {
		lineNumber++;
		colNumber = 1;
	}
}

void Lexer::EatWhitespaces() {
	while(isspace(nextChar))
		EatNext();
}

void Lexer::NextToken() {
	token = GetToken();
}

Token Lexer::GetToken() {
	tokenString = ""; //Reset the tokenstring
	EatWhitespaces();
	filePos.lineNumber = lineNumber;
	filePos.columNumber = colNumber;

	//KEYWORD, IDENTIFIER, TYPE
	//If the character begins with a letter it must be a keyword, type, or identifier
	if(isalpha(nextChar) || nextChar == '_') {
		while (isalnum(nextChar) || nextChar == '_')
			AppendNext();
		if (tokenString == "import") return Token::IMPORT;
		if (tokenString == "foreign") 	return Token::FOREGIN;
		if (tokenString == "if") 		return Token::IF;
		if (tokenString == "else") 		return Token::ELSE;
		if (tokenString == "return")	return Token::RETURN;
		return Token::Identifier;
	}
	//NUMERIC LITERAL
	if (isdigit(nextChar)) { //Character was not alpha so we already know that it will not be an identifier
		while(isdigit(nextChar) || nextChar == '.')
			AppendNext();
		return Token::Number;
	}

	//COMMENTS
	if (nextChar == '#') {
		EatNext();	//Eat the '# 'char
		while(nextChar != EOF && nextChar != '\n' && nextChar != '\r')
			EatNext();	//Now we eat the comment body itself
		//We have reached the end of the comment.  If is not the end of the file get the next token
		if(nextChar != EOF)
			return GetToken();
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
		else if (nextChar == '>') {
			AppendNext();
			return Token::TypeReturn;
		}
		else {
			return Token::TypeDeclare;
		}
	}

	//BIN OPS
	else if(nextChar == '=') {
		AppendNext();
		return Token::EQUALS;
	}
	else if(nextChar == '+') {
		AppendNext();
		if(nextChar == '=') {
			AppendNext();
			return Token::ADD_EQUALS;
		}
		return Token::ADD;
	}
	else if(nextChar == '-') {
		AppendNext();
		if(nextChar == '=') {
			AppendNext();
			return Token::SUB_EQUALS;
		}
		return Token::SUB;
	}
	else if(nextChar == '*') {
		AppendNext();
		if(nextChar == '=') {
			AppendNext();
			return Token::MUL_EQUALS;
		}
		return Token::MUL;
	}
	else if(nextChar == '/') {
		AppendNext();
		if(nextChar == '=') {
			AppendNext();
			return Token::DIV_EQUALS;
		}
		return Token::DIV;
	}
	else if(nextChar == '%') {
		AppendNext();
		if(nextChar == '=') {
			AppendNext();
			return Token::MOD_EQUALS;
		}
		return Token::MOD;
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
		return Token::UNKOWN;
	}
}
