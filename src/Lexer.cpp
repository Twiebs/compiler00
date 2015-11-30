#define USE_INDENT_BLOCK
#define INDENT_SPACE_COUNT 2

#include "Common.hpp"
#include "Build.hpp"

#include "Lexer.hpp"

const char* ToString(TokenType tokenType) {
	switch (tokenType) {
	default: return "UNIMPLEMENTED ToString(TokenType)";
	};
}

void Lexer::SetBuffer(const char* buffer) {
    this->currentChar = buffer;
    columnNumber = 1;
    lineNumber = 1;
}

void Lexer::AppendToken(char character) {
	token.string += character;
}

void Lexer::AppendCurrentChar() {
    token.string += *currentChar;
    EatCurrentChar();
}

static inline bool IsNewline (const char* current) {
	if (*current == '\r') {
		auto next = current + 1;
		auto result = *next == '\n';
		return result;
	}

	auto result = *current == '\n';
	return result;
}

void Lexer::EatCurrentChar() {
    if (IsNewline(currentChar)) {
        lineNumber++;
        columnNumber = 1;
        currentChar++;
    } else {
        currentChar++;
        columnNumber++;
    }
}

void Lexer::eatLine() {
    while(*currentChar != '\n') {
        if (*currentChar == EOF) return;
        currentChar++;
        columnNumber++;
    }
    currentChar++;
    lineNumber++;
    columnNumber = 1;
}

void Lexer::EatNewlineChars() {
	assert(*currentChar == '\n' || *currentChar == '\r');
	if (*currentChar == '\r') currentChar += 2;
	else currentChar++;
}


void Lexer::nextToken() {
#ifdef USE_INDENT_BLOCK
    if (IsNewline(currentChar)) {
		lineNumber++;
		columnNumber = 1;
		EatNewlineChars();
        if (IsNewline(currentChar)) {
            nextToken(); // Recurse through the indentBlock again
            return;
        }

        int indentLevel = 0;
        while (*currentChar == '\t' || *currentChar == ' ') {
            int spaceCount = 0;
            if (*currentChar == ' ') {
                spaceCount++;
                EatCurrentChar();
                if (spaceCount > INDENT_SPACE_COUNT) {
                    indentLevel++;
                    spaceCount = 0;
                }
            } else {
                EatCurrentChar();
                indentLevel++;
            }
        }


		token.string = "";
		token.site.lineNumber = lineNumber;
		token.site.columNumber = columnNumber;
		token.location.lineNumber = lineNumber;
		token.location.columnNumber = lineNumber;

        if (indentLevel > (int)currentIndentLevel) {
            token.type = TOKEN_BLOCK_OPEN;;
			currentIndentLevel = indentLevel;
            return;
        } else if (indentLevel < (int)currentIndentLevel) {
            token.string = "";
            token.type = TOKEN_BLOCK_CLOSE;
            currentIndentLevel = indentLevel;
            return;
        }

    }
#endif

	while (isspace(*currentChar)) EatCurrentChar();

    token.string = "";
    token.type = TOKEN_UNKOWN;
	token.location.lineNumber = lineNumber;
	token.location.columnNumber = columnNumber;

    if (isalpha(*currentChar) || *currentChar== '_') {
        while ((isalnum(*currentChar) || *currentChar == '_') && *currentChar!= '.') AppendCurrentChar();
        if (token.string == "IMPORT") 			    token.type = TOKEN_IMPORT;
        else if (token.string == "FOREIGN")	 		token.type = TOKEN_FOREIGN;
        else if (token.string == "STRUCT")			token.type = TOKEN_STRUCT;
        else if (token.string == "IF")				token.type = TOKEN_IF;
        else if (token.string == "ELSE") 			token.type = TOKEN_ELSE;
        else if (token.string == "ITER")			token.type = TOKEN_ITER;
        else if (token.string == "TO")				token.type = TOKEN_TO;
        else if (token.string == "RETURN")			token.type = TOKEN_RETURN;
        else if (token.string == "TRUE")			token.type = TOKEN_TRUE;
        else if (token.string == "FALSE")			token.type = TOKEN_FALSE;
        else token.type = TOKEN_IDENTIFIER;
    }

        // The Current token is a Numeric Literal
    else if (isdigit(*currentChar)) {
        bool decimalSeen = false;
        while (isdigit(*currentChar) || *currentChar== '.') {
            if (*currentChar== '.') {
                if (!decimalSeen)
                    decimalSeen = true;
                else
                    LOG_ERROR(token.site << "Two decimals found in numeric constant!");
            }
            AppendCurrentChar();
        }
        token.type = TOKEN_NUMBER;
    }


	else if (*currentChar == '"') {
		EatCurrentChar();
		while (*currentChar != '"') {
	        if (*currentChar == 92) {
	            EatCurrentChar();
	            if (*currentChar == 'n') { 
					EatCurrentChar();
					AppendToken('\n');
					continue;
				}
	        }
			AppendCurrentChar();
		}
		EatCurrentChar();	// Eat the "
		token.type = TOKEN_STRING;
	}


        // COMMENTS
    else if (*currentChar == '#') {
        EatCurrentChar();	//Eat the '# 'char
        while (*currentChar != EOF && *currentChar != '\n' && *currentChar != '\r')
            EatCurrentChar();	//Now we eat the comment body itself
        //We have reached the end of the comment.	If is not the end of the file get the next token
        if (*currentChar != EOF) {
            nextToken();
            return;
        }
    }

        // COLON TOKENS
    else if (*currentChar== ':') {
        AppendCurrentChar();
        if (*currentChar== ':') {
            AppendCurrentChar();
            token.type = TOKEN_TYPE_DEFINE;
        } /*else if (*currentChar == '=') {
            AppendCurrentChar();
            token.type = TOKEN_TYPE_INFER;
        } */ else {
            token.type = TOKEN_TYPE_DECLARE;
        }
    }

    else if (*currentChar == '@') {
        AppendCurrentChar();
        token.type = TOKEN_ADDRESS;
    }

    else if (*currentChar == '$') {
        AppendCurrentChar();
        token.type = TOKEN_VALUE;
    }

    else if (*currentChar == '=') {
        AppendCurrentChar();
        if (*currentChar == '=') {
            AppendCurrentChar();
            token.type = TOKEN_LOGIC_EQUAL;
        } else {
            token.type = TOKEN_EQUALS;
        }
    }

    else if (*currentChar == '+') {
        AppendCurrentChar();
        if (*currentChar == '=') {
            AppendCurrentChar();
            token.type = TOKEN_ADD_EQUALS;
        } else {
            token.type = TOKEN_ADD;
        }
    }

    else if (*currentChar == '-') {
        AppendCurrentChar();
        if (*currentChar == '=') {
            AppendCurrentChar();
            token.type = TOKEN_SUB_EQUALS;
        } else {
            token.type = TOKEN_SUB;
        }
    }

    else if (*currentChar == '*') {
        AppendCurrentChar();
        if (*currentChar == '=') {
            AppendCurrentChar();
            token.type = TOKEN_MUL_EQUALS;
        } else {
            token.type = TOKEN_MUL;
        }
    }


    else if (*currentChar == '/') {
        AppendCurrentChar();
        if (*currentChar == '=') {
            AppendCurrentChar();
            token.type = TOKEN_DIV_EQUALS;
        } else {
            token.type = TOKEN_DIV;
        }
    }

        // LOGICAL
    else if (*currentChar == '~') {
        AppendCurrentChar();
        token.type = TOKEN_LOGIC_NOT;
    }

    else if (*currentChar == '>') {
        AppendCurrentChar();
        if (*currentChar == '>') {
            AppendCurrentChar();
            token.type = TOKEN_TYPE_RETURN;
        } else if (*currentChar == '=') {
            AppendCurrentChar();
            token.type = TOKEN_LOGIC_GREATER_EQAUL;
        } else {
            token.type = TOKEN_LOGIC_GREATER;
        }
    }

    else if (*currentChar == '<') {
        AppendCurrentChar();
        if (*currentChar == '<') {
            AppendCurrentChar();
            token.type = TOKEN_CONSTRUCT;
        } else if (*currentChar == '=') {
            AppendCurrentChar();
            token.type = TOKEN_LOGIC_LESS_EQUAL;
        } else {
            token.type = TOKEN_LOGIC_LESS;
        }
    }

    else if (*currentChar == '.') {
        AppendCurrentChar();
        if (*currentChar == '.') {
            AppendCurrentChar();
            if (*currentChar == '.') {
                AppendCurrentChar();
                token.type = TOKEN_DOTDOT;
            }
        } else {
            token.type = TOKEN_ACCESS;
        }
    }

        //BRACES, BRACKETS, SUBSCRIPTS
    else if (*currentChar == '(') {
        AppendCurrentChar();
        token.type = TOKEN_PAREN_OPEN;
    } else if (*currentChar == ')') {
        AppendCurrentChar();
        token.type = TOKEN_PAREN_CLOSE;
    }	else if (*currentChar == '[') {
        AppendCurrentChar();
        token.type = TOKEN_ARRAY_OPEN;
    } else if (*currentChar == ']') {
        AppendCurrentChar();
        token.type = TOKEN_ARRAY_CLOSE;
    } else if (*currentChar == '{') {
        AppendCurrentChar();
        token.type = TOKEN_BLOCK_OPEN;
        // currentIndentLevel++;
    } else if (*currentChar == '}') {
        AppendCurrentChar();
        token.type = TOKEN_BLOCK_CLOSE;
        // currentIndentLevel++;
    } else if (*currentChar == EOF || *currentChar == 0) {
        //Dont append or ead the EOF or buffer
        token.type = TOKEN_END_OF_BUFFER;
    } else {
        AppendCurrentChar();
        token.type = TOKEN_UNKOWN;
    }
}

//internal void EatNext (Worker* worker) {
//    worker->lastChar = worker->nextChar;
//    worker->nextChar = getc(worker->file);
//    worker->colNumber++;
//    if (worker->lastChar == '\n') {
//        worker->lineNumber++;
//        worker->colNumber = 1;
//    }
//}
//
//internal void AppendNext (Worker* worker) {
//    EatNext(worker);
//    worker->token.string += worker->lastChar;
//}

//void EatLine(Worker *worker) {
//    while (worker->lastChar != '\n' || worker->lastChar != '\r') {
//        EatNext(worker);
//    }
//}
//
//void NextToken (Worker* worker) {
//#ifdef USE_INDENT_BLOCK
//		if (worker->nextChar == '\n' || worker->nextChar == '\r') {
//			EatNext(worker);
//			if (worker->nextChar == '\n' || worker->nextChar == '\r') {
//				NextToken(worker);
//				return;
//			}
//
//			int indentLevel = 0;
//			while (worker->nextChar == '\t' || worker->nextChar == ' ') {
//				int spaceCount = 0;
//				if (worker->nextChar == ' ') {
//					spaceCount++;
//					EatNext(worker);
//					if (spaceCount > INDENT_SPACE_COUNT) {
//                        indentLevel++;
//						spaceCount = 0;
//					}
//				} else {
//					EatNext(worker);
//                    indentLevel++;
//				}
//			}
//
//			if (indentLevel > worker->currentIndentLevel) {
//				worker->token.string = "";
//				worker->token.type = TOKEN_BLOCK_OPEN;;
//				worker->token.site.lineNumber = worker->lineNumber;
//				worker->token.site.columNumber = worker->colNumber;
//				worker->currentIndentLevel = indentLevel;
//				return;
//			} else if (indentLevel < worker->currentIndentLevel) {
//				worker->token.string = "";
//				worker->token.type = TOKEN_BLOCK_CLOSE;
//				worker->token.site.lineNumber = worker->lineNumber;
//				worker->token.site.columNumber = worker->colNumber;
//				worker->currentIndentLevel = indentLevel;
//				return;
//			}
//		}
//#endif
//
//		worker->token.string = "";
//		worker->token.type = TOKEN_UNKOWN;
//		worker->token.site.lineNumber = worker->lineNumber;
//		worker->token.site.columNumber = worker->colNumber;
//		while (isspace(worker->nextChar)) EatNext(worker);
//		if (isalpha(worker->nextChar) || worker->nextChar == '_') {
//			while ((isalnum(worker->nextChar) || worker->nextChar == '_') && worker->nextChar != '.') AppendNext(worker);
//			if (worker->token.string == "IMPORT") 			    worker->token.type = TOKEN_IMPORT;
//			else if (worker->token.string == "FOREIGN")	 		worker->token.type = TOKEN_FOREIGN;
//			else if (worker->token.string == "STRUCT")			worker->token.type = TOKEN_STRUCT;
//			else if (worker->token.string == "IF")				worker->token.type = TOKEN_IF;
//			else if (worker->token.string == "ELSE") 			worker->token.type = TOKEN_ELSE;
//			else if (worker->token.string == "ITER")			worker->token.type = TOKEN_ITER;
//			else if (worker->token.string == "TO")				worker->token.type = TOKEN_TO;
//			else if (worker->token.string == "RETURN")			worker->token.type = TOKEN_RETURN;
//			else if (worker->token.string == "TRUE")			worker->token.type = TOKEN_TRUE;
//			else if (worker->token.string == "FALSE")			worker->token.type = TOKEN_FALSE;
//			else worker->token.type = TOKEN_IDENTIFIER;
//		}
//
//		// The Current worker->token is a Numeric Literal
//		else if (isdigit(worker->nextChar)) {
//			bool decimalSeen = false;
//			while (isdigit(worker->nextChar) || worker->nextChar == '.') {
//				if (worker->nextChar == '.') {
//					if (!decimalSeen)
//						decimalSeen = true;
//					else
//						LOG_ERROR(worker->token.site << "Two decimals found in numeric constant!");
//				}
//				AppendNext(worker);
//			}
//			worker->token.type = TOKEN_NUMBER;
//		}
//
//		// The Current worker->token is a String Literal
//		else if (worker->nextChar == '"') {
//			EatNext(worker);
//			while (worker->nextChar != '"') {
//                if (worker->nextChar == 92) {
//                    EatNext(worker);
//                    if (worker->nextChar == 'n') worker->nextChar = '\n';
//                }
//				AppendNext(worker);
//			}
//			EatNext(worker);	// Eat the "
//			worker->token.type = TOKEN_STRING;
//		}
//
//		// COMMENTS
//		else if (worker->nextChar == '#') {
//			EatNext(worker);	//Eat the '# 'char
//			while (worker->nextChar != EOF && worker->nextChar != '\n' && worker->nextChar != '\r')
//				EatNext(worker);	//Now we eat the comment body itself
//			//We have reached the end of the comment.	If is not the end of the file get the next worker->token
//			if (worker->nextChar != EOF) {
//					NextToken(worker);
//					return;
//			}
//		}
//
//		// COLON TOKENS
//		else if (worker->nextChar == ':') {
//			AppendNext(worker);
//			if (worker->nextChar == ':') {
//				AppendNext(worker);
//				worker->token.type = TOKEN_TYPE_DEFINE;
//			} else if (worker->nextChar == '=') {
//				AppendNext(worker);
//				worker->token.type = TOKEN_TYPE_INFER;
//			} else if (worker->nextChar == '>') {
//				AppendNext(worker);
//				worker->token.type = TOKEN_TYPE_RETURN;
//			} else {
//				worker->token.type = TOKEN_TYPE_DECLARE;
//			}
//		}
//
//		else if (worker->nextChar == '@') {
//			AppendNext(worker);
//			worker->token.type = TOKEN_ADDRESS;
//		}
//
//		else if (worker->nextChar == '$') {
//			AppendNext(worker);
//			worker->token.type = TOKEN_VALUE;
//		}
//
//		else if (worker->nextChar == '=') {
//			AppendNext(worker);
//			if (worker->nextChar == '=') {
//				AppendNext(worker);
//				worker->token.type == TOKEN_LOGIC_EQUAL;
//			} else {
//					worker->token.type = TOKEN_EQUALS;
//			}
//		}
//
//        else if (worker->nextChar == '>') {
//            AppendNext(worker);
//            if (worker->nextChar == '=') {
//                AppendNext(worker);
//                worker->token.type = TOKEN_LOGIC_GREATER_EQAUL;
//            } else if (worker->nextChar == '>') {
//				AppendNext(worker);
//				worker->token.type = TOKEN_TYPE_RETURN;
//			} else {
//                worker->token.type = TOKEN_LOGIC_GREATER;
//            }
//        }
//
//        else if (worker->nextChar == '<') {
//            AppendNext(worker);
//            if (worker->nextChar == '=') {
//                AppendNext(worker);
//                worker->token.type = TOKEN_LOGIC_LESS_EQUAL;
//            } else {
//                worker->token.type = TOKEN_LOGIC_LESS;
//            }
//        }
//
//		else if (worker->nextChar == '+') {
//			AppendNext(worker);
//			if (worker->nextChar == '=') {
//				AppendNext(worker);
//				worker->token.type = TOKEN_ADD_EQUALS;
//			} else {
//				worker->token.type = TOKEN_ADD;
//			}
//		}
//
//		else if (worker->nextChar == '-') {
//			AppendNext(worker);
//			if (worker->nextChar == '=') {
//				AppendNext(worker);
//				worker->token.type = TOKEN_SUB_EQUALS;
//			} else {
//				worker->token.type = TOKEN_SUB;
//			}
//		}
//
//		else if (worker->nextChar == '*') {
//			AppendNext(worker);
//			if (worker->nextChar == '=') {
//				AppendNext(worker);
//				worker->token.type = TOKEN_MUL_EQUALS;
//			} else {
//				worker->token.type = TOKEN_MUL;
//			}
//		}
//
//
//		else if (worker->nextChar == '/') {
//			AppendNext(worker);
//			if (worker->nextChar == '=') {
//				AppendNext(worker);
//				worker->token.type = TOKEN_DIV_EQUALS;
//			} else {
//				worker->token.type = TOKEN_DIV;
//			}
//		}
//
//		// LOGICAL
//		else if (worker->nextChar == '~') {
//			AppendNext(worker);
//			worker->token.type = TOKEN_LOGIC_NOT;
//		}
//
//
//		// REMOVE NEED FOR MODULUS BECAUSE FUCK THAT
//		// Mod(5, 2) would be better!
//		// else if (worker->nextChar == '%') {
//		// 	AppendNext(worker);
//		// 	if (worker->nextChar == '=') {
//		// 		AppendNext(worker);
//		// 		worker->token.type = TOKEN_MOD_EQUALS;
//		// 	} else {
//		// 		worker->token.type = TOKEN_MOD;
//		// 	}
//		// }
//
//		else if (worker->nextChar == '.') {
//			AppendNext(worker);
//            if (worker->nextChar == '.') {
//                AppendNext(worker);
//                if (worker->nextChar == '.') {
//                    AppendNext(worker);
//                    worker->token.type = TOKEN_DOTDOT;
//                }
//            } else {
//                worker->token.type = TOKEN_ACCESS;
//            }
//		}
//
//		//BRACES, BRACKETS, SUBSCRIPTS
//		else if (worker->nextChar == '(') {
//			AppendNext(worker);
//			worker->token.type = TOKEN_PAREN_OPEN;
//		} else if (worker->nextChar == ')') {
//			AppendNext(worker);
//			worker->token.type = TOKEN_PAREN_CLOSE;
//		}	else if (worker->nextChar == '[') {
//				AppendNext(worker);
//				worker->token.type = TOKEN_ARRAY_OPEN;
//			} else if (worker->nextChar == ']') {
//				AppendNext(worker);
//				worker->token.type = TOKEN_ARRAY_CLOSE;
//		} else if (worker->nextChar == '{') {
//			AppendNext(worker);
//			worker->token.type = TOKEN_BLOCK_OPEN;
//			// currentIndentLevel++;
//		} else if (worker->nextChar == '}') {
//			AppendNext(worker);
//			worker->token.type = TOKEN_BLOCK_CLOSE;
//			// currentIndentLevel++;
//		} else if (worker->nextChar == EOF) {
//			//Dont append or ead the EOF
//			worker->token.type = TOKEN_EOF;
//		} else {
//			AppendNext(worker);
//			worker->token.type = TOKEN_UNKOWN;
//		}
//}
