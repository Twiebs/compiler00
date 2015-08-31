#include "Lexer.hpp"

#define INDENT_SPACE_COUNT 2
int Lexer::GetIndentLevel() {
	int indentLevel = 0;
	if (nextChar == '\t' || nextChar == '\r') {
		eatNextChar();
		indentLevel++;
	} else if (nextChar == ' ') {
		int spaceCount = 1;
		eatNextChar();
		while(nextChar == ' ') {
			spaceCount++;
			eatNextChar();
			if(spaceCount > INDENT_SPACE_COUNT) {
				indentLevel++;
				spaceCount = 0;
			}
		}
	}
	return indentLevel;
}

void Lexer::next(bool statement) {
	token.string = "";
	token.type = TOKEN_UNKOWN;
	token.site.lineNumber = lineNumber;
	token.site.columNumber = colNumber;

#ifdef USE_SCOPE_INDENT
	if(statement) {
		int indentLevel = GetIndentLevel();
		if (indentLevel > currentIndentLevel) {
			token.type = TOKEN_SCOPE_OPEN;
		} else if (indentLevel < currentIndentLevel) {
			token.type = TOKEN_SCOPE_CLOSE;
		}
		return;
	}
#endif

	while (isspace(nextChar)) eatNextChar();	// Eat the whitespaces

	// The Current Token is an Identifier or a Language Keyword
	if (isalpha(nextChar) || nextChar == '_') {
		while ((isalnum(nextChar) || nextChar == '_') && nextChar != '.') appendNextChar();
		if 		(token.string == "import") 			token.type = TOKEN_IMPORT;
		else if (token.string == "foreign")	 	token.type = TOKEN_FOREIGN;
		else if (token.string == "struct")		token.type = TOKEN_STRUCT;
		else if (token.string == "if")				token.type = TOKEN_IF;
		else if (token.string == "else") 		  token.type = TOKEN_ELSE;
		else if (token.string == "iter")			token.type = TOKEN_ITER;
		else if (token.string == "to")				token.type = TOKEN_TO;
		else if (token.string == "return")		token.type = TOKEN_RETURN;
		else token.type = TOKEN_IDENTIFIER;
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
		token.type = TOKEN_NUMBER;
	}

	// The Current Token is a String Literal
	else if (this->nextChar == '"') {
		eatNextChar();
		while (this->nextChar != '"') {
			appendNextChar();
		}
		eatNextChar();	// Eat the "
		token.type = TOKEN_STRING;
	}

	// COMMENTS
	else if (this->nextChar == '#') {
		eatNextChar();	//Eat the '# 'char
		while (this->nextChar != EOF && this->nextChar != '\n' && this->nextChar != '\r')
			eatNextChar();	//Now we eat the comment body itself
		//We have reached the end of the comment.  If is not the end of the file get the next token
		if (this->nextChar != EOF)
			return next();
	}

	// COLON TOKENS
	else if (this->nextChar == ':') {
		appendNextChar();
		if (this->nextChar == ':') {
			appendNextChar();
			token.type = TOKEN_TYPE_DEFINE;
		} else if (this->nextChar == '=') {
			appendNextChar();
			token.type = TOKEN_TYPE_INFER;
		} else if (this->nextChar == '>') {
			appendNextChar();
			token.type = TOKEN_TYPE_RETURN;
		} else {
			token.type = TOKEN_TYPE_DECLARE;
		}
	}

	else if (nextChar == '@') {
		appendNextChar();
		token.type = TOKEN_POINTER;
	}

	else if (nextChar == '$') {
		appendNextChar();
		token.type = TOKEN_DEREF;
	}

	// BIN OPS
	else if (this->nextChar == '=') {
		appendNextChar();
		token.type = TOKEN_EQUALS;
	}

	else if (this->nextChar == '+') {
		appendNextChar();
		if (this->nextChar == '=') {
			appendNextChar();
			token.type = TOKEN_ADD_EQUALS;
		} else {
			token.type = TOKEN_ADD;
		}
	}

	else if (this->nextChar == '-') {
		appendNextChar();
		if (this->nextChar == '=') {
			appendNextChar();
			token.type = TOKEN_SUB_EQUALS;
		} else {
			token.type = TOKEN_SUB;
		}
	}

	else if (this->nextChar == '*') {
		appendNextChar();
		if (this->nextChar == '=') {
			appendNextChar();
			token.type = TOKEN_MUL_EQUALS;
		} else {
			token.type = TOKEN_MUL;
		}
	}


	else if (this->nextChar == '/') {
		appendNextChar();
		if (this->nextChar == '=') {
			appendNextChar();
			token.type = TOKEN_DIV_EQUALS;
		} else {
			token.type = TOKEN_DIV;
		}
	}

	else if (this->nextChar == '%') {
		appendNextChar();
		if (this->nextChar == '=') {
			appendNextChar();
			token.type = TOKEN_MOD_EQUALS;
		} else {
			token.type = TOKEN_MOD;
		}
	}

	else if (nextChar == '.') {
		appendNextChar();
		token.type = TOKEN_ACCESS;
	}

	//BRACES, BRACKETS, SUBSCRIPTS
	else if (this->nextChar == '(') {
		appendNextChar();
		token.type = TOKEN_PAREN_OPEN;
	} else if (this->nextChar == ')') {
		appendNextChar();
		token.type = TOKEN_PAREN_CLOSE;
	}	else if (this->nextChar == '[') {
			appendNextChar();
			token.type = TOKEN_ARRAY_OPEN;
		} else if (this->nextChar == ']') {
			appendNextChar();
			token.type = TOKEN_ARRAY_CLOSE;
	} else if (this->nextChar == '{') {
		appendNextChar();
		token.type = TOKEN_SCOPE_OPEN;
	} else if (this->nextChar == '}') {
		appendNextChar();
		token.type = TOKEN_SCOPE_CLOSE;
	} else if (this->nextChar == EOF) {
		//Dont append or ead the EOF
		token.type = TOKEN_EOF;
	} else {
		appendNextChar();
		token.type = TOKEN_UNKOWN;
	}
}

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

//Token GetNextToken(LexState& state) {
//	while(isspace(state.nextChar)) EatNextChar(state);	//Eat the whitespaces
//
//	// Set the site of the token and default its values
//	Token token = {};
//	token.string = "";
//	token.type = TOKEN_UNKOWN;
//	token.site.lineNumber = state.lineNumber;
//	token.site.columNumber = state.colNumber;
//
//	// The Current Token is an Identifier or a Language Keyword
//	if(isalpha(state.nextChar) || state.nextChar == '_') {
//		while (isalnum(state.nextChar) || state.nextChar == '_') AppendToken(token, state);
//		if 			(token.string == "import") 		token.type = TOKEN_IMPORT;
//		else if (token.string == "foreign")	 	token.type = TOKEN_FOREIGN;
//		else if (token.string == "if")				token.type = TOKEN_IF;
//		else if (token.string == "else") 		  token.type = TOKEN_ELSE;
//		else if (token.string == "for") 		  token.type = TOKEN_FOR;
//		else if (token.string == "while") 		token.type = TOKEN_WHILE;
//		else if (token.string == "return")		token.type = TOKEN_RETURN;
//		else token.type = TOKEN_IDENTIFIER;
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
//		token.type = TOKEN_NUMBER;
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
//		token.type = TOKEN_STRING;
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
//			token.type = TOKEN_TYPE_DEFINE;
//		}
//		else if (state.nextChar == '='){
//			AppendToken(token, state);
//			token.type = TOKEN_TYPE_INFER;
//		}
//		else if (state.nextChar == '>') {
//			AppendToken(token, state);
//			token.type = TOKEN_TYPE_RETURN;
//		}
//		else {
//			token.type = TOKEN_TYPE_DECLARE;
//		}
//	}
//
//	//BIN OPS
//	else if(state.nextChar == '=') {
//		AppendToken(token, state);
//		token.type = TOKEN_EQUALS;
//	}
//	else if(state.nextChar == '+') {
//		AppendToken(token, state);
//		if(state.nextChar == '=') {
//			AppendToken(token, state);
//			token.type = TOKEN_ADD_EQUALS;
//		} else {
//			token.type = TOKEN_ADD;
//		}
//	}
//	else if(state.nextChar == '-') {
//		AppendToken(token, state);
//		if(state.nextChar == '=') {
//			AppendToken(token, state);
//			token.type = TOKEN_SUB_EQUALS;
//		} else {
//			token.type = TOKEN_SUB;
//		}
//	}
//
//	else if(state.nextChar == '*') {
//		AppendToken(token, state);
//		if(state.nextChar == '=') {
//			AppendToken(token, state);
//			token.type = TOKEN_MUL_EQUALS;
//		} else {
//			token.type = TOKEN_MUL;
//		}
//	}
//
//
//	else if(state.nextChar == '/') {
//		AppendToken(token, state);
//		if(state.nextChar == '=') {
//			AppendToken(token, state);
//			token.type = TOKEN_DIV_EQUALS;
//		} else {
//			token.type = TOKEN_DIV;
//		}
//	}
//
//	else if(state.nextChar == '%') {
//		AppendToken(token, state);
//		if(state.nextChar == '=') {
//			AppendToken(token, state);
//			token.type = TOKEN_MOD_EQUALS;
//		} else {
//			token.type = TOKEN_MOD;
//		}
//	}
//
//	//BRACES, BRACKETS, SUBSCRIPTS
//	else if (state.nextChar == '(') {
//		AppendToken(token, state);
//		token.type = TOKEN_PAREN_OPEN;
//	}
//	else if (state.nextChar == ')') {
//		AppendToken(token, state);
//		token.type = TOKEN_PAREN_CLOSE;
//	}
//	else if (state.nextChar == '{') {
//		AppendToken(token, state);
//		token.type = TOKEN_SCOPE_OPEN;
//	}
//	else if (state.nextChar == '}') {
//		AppendToken(token, state);
//		token.type = TOKEN_SCOPE_CLOSE;
//	}
//	else if (state.nextChar == EOF) {
//		//Dont append or ead the EOF
//		token.type = TOKEN_EOF;
//	}
//	else{
//		AppendToken(token, state);
//		token.type = TOKEN_UNKOWN;
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
