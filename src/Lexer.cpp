#include "Lexer.hpp"

//TODO EastNextChar: this doesnt look like it will set the lineNumber and the colum number correctly
//void EatNextChar(LexState& state) {
//	lastChar = nextChar;
//	nextChar = stream.get();
//	colNumber++;
//	if(lastChar == '\n') {
//		lineNumber++;
//		colNumber = 1;
//	}
//}
//
//void AppendToken(Token& token, LexState& state) {
//	lastChar = nextChar;
//	nextChar = stream.get();
//	colNumber++;
//	if (lastChar == '\n') {
//		lineNumber++;
//		colNumber = 1;
//	} else if (lastChar != ' ') {
//		token.string += lastChar;
//	}
//}


void Lexer::eatNextChar() {
	lastChar = nextChar;
	nextChar = stream.get();
	colNumber++;
	if( lastChar == '\n') {
		 lineNumber++;
		 colNumber = 1;
	}
}

void Lexer::appendNextChar() {
	lastChar =  nextChar;
	nextChar = stream.get();
	colNumber++;
	if (lastChar == '\n') {
		lineNumber++;
		colNumber = 1;
	} else if (lastChar != ' ') {
		token.string += lastChar;
	}
}

void Lexer::next() {
	while(isspace(nextChar)) eatNextChar();	//Eat the whitespaces

	token.string = "";
	token.type = TokenType::UNKOWN;
	token.site.lineNumber = lineNumber;
	token.site.columNumber = colNumber;

	// The Current Token is an Identifier or a Language Keyword
	if(isalpha(nextChar) || nextChar == '_') {
		while (isalnum(nextChar) || nextChar == '_') appendNextChar();
		if 			(token.string == "import") 		token.type = TokenType::IMPORT;
		else if (token.string == "foreign")	 	token.type = TokenType::FOREIGN;
		else if (token.string == "if")				token.type = TokenType::IF;
		else if (token.string == "else") 		  token.type = TokenType::ELSE;
		else if (token.string == "for") 		  token.type = TokenType::FOR;
		else if (token.string == "while") 		token.type = TokenType::WHILE;
		else if (token.string == "return")		token.type = TokenType::RETURN;
		else token.type = TokenType::IDENTIFIER;
	}

	// The Current Token is a Numeric Literal
	else if (isdigit(this->nextChar)) {
		bool decimalSeen = false;
		while (isdigit(this->nextChar) || this->nextChar == '.') {
			if (this->nextChar == '.') {
				if (!decimalSeen)
					decimalSeen = true;
				else
					LOG_ERROR(token.site << "Two decimals found in numeric constant!");
			}
			appendNextChar();
		}
		token.type = TokenType::NUMBER;
	}

	// The Current Token is a String Literal
	else if (this->nextChar == '"') {
		eatNextChar();
		while (this->nextChar != '"') {
			appendNextChar();
		}
		eatNextChar();	//Eat the "
		token.type = TokenType::STRING;
	}

		//COMMENTS
	else if (this->nextChar == '#') {
		eatNextChar();	//Eat the '# 'char
		while(this->nextChar != EOF && this->nextChar != '\n' && this->nextChar != '\r')
			eatNextChar();	//Now we eat the comment body itself
		//We have reached the end of the comment.  If is not the end of the file get the next token
		if(this->nextChar != EOF)
			return next();
	}

	// COLON TOKENS
	else if (this->nextChar == ':') {
		appendNextChar();
		if (this->nextChar == ':') {
			appendNextChar();
			token.type = TokenType::TYPE_DEFINE;
		} else if (this->nextChar == '=') {
			appendNextChar();
			token.type = TokenType::TYPE_INFER;
		} else if (this->nextChar == '>') {
			appendNextChar();
			token.type = TokenType::TYPE_RETURN;
		} else {
			token.type = TokenType::TYPE_DECLARE;
		}
	}

	//BIN OPS
	else if (this->nextChar == '=') {
		appendNextChar();
		token.type = TokenType::EQUALS;
	}

	else if (this->nextChar == '+') {
		appendNextChar();
		if (this->nextChar == '=') {
			appendNextChar();
			token.type = TokenType::ADD_EQUALS;
		} else {
			token.type = TokenType::ADD;
		}
	}

	else if (this->nextChar == '-') {
		appendNextChar();
		if (this->nextChar == '=') {
			appendNextChar();
			token.type = TokenType::SUB_EQUALS;
		} else {
			token.type = TokenType::SUB;
		}
	}

	else if (this->nextChar == '*') {
		appendNextChar();
		if (this->nextChar == '=') {
			appendNextChar();
			token.type = TokenType::MUL_EQUALS;
		} else {
			token.type = TokenType::MUL;
		}
	}


	else if (this->nextChar == '/') {
		appendNextChar();
		if (this->nextChar == '=') {
			appendNextChar();
			token.type = TokenType::DIV_EQUALS;
		} else {
			token.type = TokenType::DIV;
		}
	}

	else if (this->nextChar == '%') {
		appendNextChar();
		if (this->nextChar == '=') {
			appendNextChar();
			token.type = TokenType::MOD_EQUALS;
		} else {
			token.type = TokenType::MOD;
		}
	}

	//BRACES, BRACKETS, SUBSCRIPTS
	else if (this->nextChar == '(') {
		appendNextChar();
		token.type = TokenType::ParenOpen;
	} else if (this->nextChar == ')') {
		appendNextChar();
		token.type = TokenType::ParenClose;
	} else if (this->nextChar == '{') {
		appendNextChar();
		token.type = TokenType::ScopeOpen;
	} else if (this->nextChar == '}') {
		appendNextChar();
		token.type = TokenType::ScopeClose;
	} else if (this->nextChar == EOF) {
		//Dont append or ead the EOF
		token.type = TokenType::END_OF_FILE;
	} else {
		appendNextChar();
		token.type = TokenType::UNKOWN;
	}
}


//Token GetNextToken(LexState& state) {
//	while(isspace(state.nextChar)) EatNextChar(state);	//Eat the whitespaces
//
//	// Set the site of the token and default its values
//	Token token = {};
//	token.string = "";
//	token.type = TokenType::UNKOWN;
//	token.site.lineNumber = state.lineNumber;
//	token.site.columNumber = state.colNumber;
//
//	// The Current Token is an Identifier or a Language Keyword
//	if(isalpha(state.nextChar) || state.nextChar == '_') {
//		while (isalnum(state.nextChar) || state.nextChar == '_') AppendToken(token, state);
//		if 			(token.string == "import") 		token.type = TokenType::IMPORT;
//		else if (token.string == "foreign")	 	token.type = TokenType::FOREIGN;
//		else if (token.string == "if")				token.type = TokenType::IF;
//		else if (token.string == "else") 		  token.type = TokenType::ELSE;
//		else if (token.string == "for") 		  token.type = TokenType::FOR;
//		else if (token.string == "while") 		token.type = TokenType::WHILE;
//		else if (token.string == "return")		token.type = TokenType::RETURN;
//		else token.type = TokenType::IDENTIFIER;
//		return token;
//	}
//
//	// The Current Token is a Numeric Literal
//	if (isdigit(state.nextChar)) {
//		bool decimalSeen = false;
//		while(isdigit(state.nextChar) || state.nextChar == '.') {
//			if(state.nextChar == '.') {
//				if(!decimalSeen) decimalSeen = true;
//				else LOG_ERROR(token.site << "Two decimals found in numeric constant!");
//			}
//			AppendToken(token, state);
//		}
//		token.type = TokenType::NUMBER;
//		return token;
//	}
//
//	// The Current Token is a String Literal
//	if (state.nextChar == '"') {
//		EatNextChar(state);
//		while (state.nextChar != '"') {
//			AppendToken(token, state);
//		}
//		EatNextChar(state);	//Eat the "
//		token.type = TokenType::STRING;
//		return token;
//	}
//
//	//COMMENTS
//	if (state.nextChar == '#') {
//		EatNextChar(state);	//Eat the '# 'char
//		while(state.nextChar != EOF && state.nextChar != '\n' && state.nextChar != '\r')
//			EatNextChar(state);	//Now we eat the comment body itself
//		//We have reached the end of the comment.  If is not the end of the file get the next token
//		if(state.nextChar != EOF)
//			return GetNextToken(state);
//	}
//
//	//COLON TOKENS
//	if (state.nextChar == ':') {
//		AppendToken(token, state);
//		if(state.nextChar == ':') {
//			AppendToken(token, state);
//			token.type = TokenType::TYPE_DEFINE;
//		}
//		else if (state.nextChar == '='){
//			AppendToken(token, state);
//			token.type = TokenType::TYPE_INFER;
//		}
//		else if (state.nextChar == '>') {
//			AppendToken(token, state);
//			token.type = TokenType::TYPE_RETURN;
//		}
//		else {
//			token.type = TokenType::TYPE_DECLARE;
//		}
//	}
//
//	//BIN OPS
//	else if(state.nextChar == '=') {
//		AppendToken(token, state);
//		token.type = TokenType::EQUALS;
//	}
//	else if(state.nextChar == '+') {
//		AppendToken(token, state);
//		if(state.nextChar == '=') {
//			AppendToken(token, state);
//			token.type = TokenType::ADD_EQUALS;
//		} else {
//			token.type = TokenType::ADD;
//		}
//	}
//	else if(state.nextChar == '-') {
//		AppendToken(token, state);
//		if(state.nextChar == '=') {
//			AppendToken(token, state);
//			token.type = TokenType::SUB_EQUALS;
//		} else {
//			token.type = TokenType::SUB;
//		}
//	}
//
//	else if(state.nextChar == '*') {
//		AppendToken(token, state);
//		if(state.nextChar == '=') {
//			AppendToken(token, state);
//			token.type = TokenType::MUL_EQUALS;
//		} else {
//			token.type = TokenType::MUL;
//		}
//	}
//
//
//	else if(state.nextChar == '/') {
//		AppendToken(token, state);
//		if(state.nextChar == '=') {
//			AppendToken(token, state);
//			token.type = TokenType::DIV_EQUALS;
//		} else {
//			token.type = TokenType::DIV;
//		}
//	}
//
//	else if(state.nextChar == '%') {
//		AppendToken(token, state);
//		if(state.nextChar == '=') {
//			AppendToken(token, state);
//			token.type = TokenType::MOD_EQUALS;
//		} else {
//			token.type = TokenType::MOD;
//		}
//	}
//
//	//BRACES, BRACKETS, SUBSCRIPTS
//	else if (state.nextChar == '(') {
//		AppendToken(token, state);
//		token.type = TokenType::ParenOpen;
//	}
//	else if (state.nextChar == ')') {
//		AppendToken(token, state);
//		token.type = TokenType::ParenClose;
//	}
//	else if (state.nextChar == '{') {
//		AppendToken(token, state);
//		token.type = TokenType::ScopeOpen;
//	}
//	else if (state.nextChar == '}') {
//		AppendToken(token, state);
//		token.type = TokenType::ScopeClose;
//	}
//	else if (state.nextChar == EOF) {
//		//Dont append or ead the EOF
//		token.type = TokenType::END_OF_FILE;
//	}
//	else{
//		AppendToken(token, state);
//		token.type = TokenType::UNKOWN;
//	}
//	state.token = token;
//	return token;
//}

#if 0

Lexer::Lexer(std::string filename) {
	this->filePos.filename = filename;
	stream.open(std::string(rootDir + filename));
	if(!stream.is_open()) {
		std::cout << "Error: Can not open file: " << rootDir + filename << "\n";
		abort();
	}
	this->nextChar = stream.peek();
	this->lastChar = stream.get();	//Wait does lastChar actualy meen "lastChar" //We probably dont need
	//to store the nextChar anywhere
}

Lexer::~Lexer() {
	//lol... the filehandle was never closed!
}

void Lexer::AppendNext() {
	lastChar = nextChar;
	nextChar = stream.get();
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
	nextChar = stream.get();

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

	//If the character begins with a letter it must be a keyword, type, or identifier
	// NOTE @KEYWORD, @IDENTIFIER, @TYPE
	if(isalpha(nextChar) || nextChar == '_') {
		while (isalnum(nextChar) || nextChar == '_') AppendNext();
		if (tokenString == "import") 	return Token::IMPORT;
		if (tokenString == "foreign") 	return Token::FOREIGN;
		if (tokenString == "if") 		return Token::IF;
		if (tokenString == "else") 		return Token::ELSE;
		if (tokenString == "for")		return Token::FOR;
		if (tokenString == "while")		return Token::WHILE;
		if (tokenString == "return")	return Token::RETURN;
		return Token::IDENTIFIER;	//The tokenString did not match a language keyword we assume it must be an identifier
	}

	// NOTE @NUMERIC LITERAL
	if (isdigit(nextChar)) { // Character was not alpha so we already know that it will not be an identifier
		bool decimalSeen = false;
		while(isdigit(nextChar) || nextChar == '.') {
			if(nextChar == '.') {
				if(!decimalSeen) decimalSeen = true;
				else LOG_ERROR(filePos << "Two decimals found in numeric constant!");
			}
			AppendNext();
		}
		return Token::Number;
	}


	// NOTE STRING LITERAL
	if(nextChar == '"') {
		EatNext();	//Eat the "
		while(nextChar != '"') {
			AppendNext();
		}
		EatNext();	//Eat the "
		return Token::STRING;
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

#endif
