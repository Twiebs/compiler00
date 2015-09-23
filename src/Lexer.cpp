#define USE_INDENT_BLOCK
#define INDENT_SPACE_COUNT 2

#include "Common.hpp"
#include "Build.hpp"

internal void EatNext (Worker* worker) {
	worker->lastChar = worker->nextChar;
	worker->nextChar = getc(worker->file);
	worker->colNumber++;
	if (worker->lastChar == '\n') {
		 worker->lineNumber++;
		 worker->colNumber = 1;
	}
}

internal void AppendNext (Worker* worker) {
	worker->lastChar =	worker->nextChar;
	worker->nextChar = getc(worker->file);
	worker->colNumber++;
	if (worker->lastChar == '\n') {
		worker->lineNumber++;
		worker->colNumber = 1;
	} else {
		worker->token.string += worker->lastChar;
	}
}

void EatLine (Worker* worker) {
	while(worker->lastChar != '\n' || worker->lastChar != '\r') {
		EatNext(worker);
	}
}

void NextToken (Worker* worker) {
#ifdef USE_INDENT_BLOCK
		if (worker->nextChar == '\n' || worker->nextChar == '\r') {
			EatNext(worker);
			if (worker->nextChar == '\n' || worker->nextChar == '\r') {
				NextToken(worker);
				return;
			}

			int indentCount = 0;
			while (worker->nextChar == '\t' || worker->nextChar == ' ') {
				int spaceCount = 0;
				if (worker->nextChar == ' ') {
					spaceCount++;
					EatNext(worker);
					if (spaceCount > INDENT_SPACE_COUNT) {
						indentCount++;
						spaceCount = 0;
					}
				} else {
					EatNext(worker);
					indentCount++;
				}
			}

			auto indentLevel = indentCount;	// @Cleanup
			if (indentLevel > worker->currentIndentLevel) {
				worker->token.string = "";
				worker->token.type = TOKEN_SCOPE_OPEN;;
				worker->token.site.lineNumber = worker->lineNumber;
				worker->token.site.columNumber = worker->colNumber;
				worker->currentIndentLevel = indentLevel;
				return;
			} else if (indentLevel < worker->currentIndentLevel) {
				worker->token.string = "";
				worker->token.type = TOKEN_SCOPE_CLOSE;
				worker->token.site.lineNumber = worker->lineNumber;
				worker->token.site.columNumber = worker->colNumber;
				worker->currentIndentLevel = indentLevel;
				return;
			}
		}
#endif

		worker->token.string = "";
		worker->token.type = TOKEN_UNKOWN;
		worker->token.site.lineNumber = worker->lineNumber;
		worker->token.site.columNumber = worker->colNumber;
		while (isspace(worker->nextChar)) EatNext(worker);
		if (isalpha(worker->nextChar) || worker->nextChar == '_') {
			while ((isalnum(worker->nextChar) || worker->nextChar == '_') && worker->nextChar != '.') AppendNext(worker);
			if 		(worker->token.string == "IMPORT") 			worker->token.type = TOKEN_IMPORT;
			else if (worker->token.string == "FOREIGN")	 		worker->token.type = TOKEN_FOREIGN;
			else if (worker->token.string == "STRUCT")			worker->token.type = TOKEN_STRUCT;
			else if (worker->token.string == "IF")				worker->token.type = TOKEN_IF;
			else if (worker->token.string == "ELSE") 			worker->token.type = TOKEN_ELSE;
			else if (worker->token.string == "ITER")			worker->token.type = TOKEN_ITER;
			else if (worker->token.string == "TO")				worker->token.type = TOKEN_TO;
			else if (worker->token.string == "RETURN")			worker->token.type = TOKEN_RETURN;
			else if (worker->token.string == "TRUE")			worker->token.type = TOKEN_TRUE;
			else if (worker->token.string == "FALSE")			worker->token.type = TOKEN_FALSE;
			else worker->token.type = TOKEN_IDENTIFIER;
		}

		// The Current worker->token is a Numeric Literal
		else if (isdigit(worker->nextChar)) {
			bool decimalSeen = false;
			while (isdigit(worker->nextChar) || worker->nextChar == '.') {
				if (worker->nextChar == '.') {
					if (!decimalSeen)
						decimalSeen = true;
					else
						LOG_ERROR(worker->token.site << "Two decimals found in numeric constant!");
				}
				AppendNext(worker);
			}
			worker->token.type = TOKEN_NUMBER;
		}

		// The Current worker->token is a String Literal
		else if (worker->nextChar == '"') {
			EatNext(worker);
			while (worker->nextChar != '"') {
				AppendNext(worker);
			}
			EatNext(worker);	// Eat the "
			worker->token.type = TOKEN_STRING;
		}

		// COMMENTS
		else if (worker->nextChar == '#') {
			EatNext(worker);	//Eat the '# 'char
			while (worker->nextChar != EOF && worker->nextChar != '\n' && worker->nextChar != '\r')
				EatNext(worker);	//Now we eat the comment body itself
			//We have reached the end of the comment.	If is not the end of the file get the next worker->token
			if (worker->nextChar != EOF) {
					NextToken(worker);
					return;
			}
		}

		// COLON TOKENS
		else if (worker->nextChar == ':') {
			AppendNext(worker);
			if (worker->nextChar == ':') {
				AppendNext(worker);
				worker->token.type = TOKEN_TYPE_DEFINE;
			} else if (worker->nextChar == '=') {
				AppendNext(worker);
				worker->token.type = TOKEN_TYPE_INFER;
			} else if (worker->nextChar == '>') {
				AppendNext(worker);
				worker->token.type = TOKEN_TYPE_RETURN;
			} else {
				worker->token.type = TOKEN_TYPE_DECLARE;
			}
		}

		else if (worker->nextChar == '@') {
			AppendNext(worker);
			worker->token.type = TOKEN_ADDRESS;
		}

		else if (worker->nextChar == '$') {
			AppendNext(worker);
			worker->token.type = TOKEN_VALUE;
		}

		else if (worker->nextChar == '=') {
			AppendNext(worker);
			if (worker->nextChar == '=') {
				AppendNext(worker);
				worker->token.type == TOKEN_BOOLEAN_EQUAL;
			} else {
					worker->token.type = TOKEN_EQUALS;
			}
		}

		else if (worker->nextChar == '+') {
			AppendNext(worker);
			if (worker->nextChar == '=') {
				AppendNext(worker);
				worker->token.type = TOKEN_ADD_EQUALS;
			} else {
				worker->token.type = TOKEN_ADD;
			}
		}

		else if (worker->nextChar == '-') {
			AppendNext(worker);
			if (worker->nextChar == '=') {
				AppendNext(worker);
				worker->token.type = TOKEN_SUB_EQUALS;
			} else {
				worker->token.type = TOKEN_SUB;
			}
		}

		else if (worker->nextChar == '*') {
			AppendNext(worker);
			if (worker->nextChar == '=') {
				AppendNext(worker);
				worker->token.type = TOKEN_MUL_EQUALS;
			} else {
				worker->token.type = TOKEN_MUL;
			}
		}


		else if (worker->nextChar == '/') {
			AppendNext(worker);
			if (worker->nextChar == '=') {
				AppendNext(worker);
				worker->token.type = TOKEN_DIV_EQUALS;
			} else {
				worker->token.type = TOKEN_DIV;
			}
		}

		// LOGICAL
		else if (worker->nextChar == '~') {
			AppendNext(worker);
			worker->token.type = TOKEN_LOGIC_NOT;
		}


		// REMOVE NEED FOR MODULUS BECAUSE FUCK THAT
		// Mod(5, 2) would be better!
		// else if (worker->nextChar == '%') {
		// 	AppendNext(worker);
		// 	if (worker->nextChar == '=') {
		// 		AppendNext(worker);
		// 		worker->token.type = TOKEN_MOD_EQUALS;
		// 	} else {
		// 		worker->token.type = TOKEN_MOD;
		// 	}
		// }

		else if (worker->nextChar == '.') {
			AppendNext(worker);
			worker->token.type = TOKEN_ACCESS;
		}

		//BRACES, BRACKETS, SUBSCRIPTS
		else if (worker->nextChar == '(') {
			AppendNext(worker);
			worker->token.type = TOKEN_PAREN_OPEN;
		} else if (worker->nextChar == ')') {
			AppendNext(worker);
			worker->token.type = TOKEN_PAREN_CLOSE;
		}	else if (worker->nextChar == '[') {
				AppendNext(worker);
				worker->token.type = TOKEN_ARRAY_OPEN;
			} else if (worker->nextChar == ']') {
				AppendNext(worker);
				worker->token.type = TOKEN_ARRAY_CLOSE;
		} else if (worker->nextChar == '{') {
			AppendNext(worker);
			worker->token.type = TOKEN_SCOPE_OPEN;
			// currentIndentLevel++;
		} else if (worker->nextChar == '}') {
			AppendNext(worker);
			worker->token.type = TOKEN_SCOPE_CLOSE;
			// currentIndentLevel++;
		} else if (worker->nextChar == EOF) {
			//Dont append or ead the EOF
			worker->token.type = TOKEN_EOF;
		} else {
			AppendNext(worker);
			worker->token.type = TOKEN_UNKOWN;
		}
}
