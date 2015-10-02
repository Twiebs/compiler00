#define USE_INDENT_BLOCK
#define INDENT_SPACE_COUNT 2

#include "Common.hpp"
#include "Build.hpp"

#include "Lexer.hpp"

// This is probably a terrible idea the interpeter and the static compiler will
// be vastly different from the way that they handle expresions and do stuff like that
// If you were to type x it would be a serious comipler error but the interpreter should tell you exactly what that
// Thing is.

#if 1


void InterpLexer::begin() {
    if (buffer.size() > 0) {
        nextChar = buffer[0];
    } else {
        nextChar = EOF;
    }
    bufferPos = 1;
}

void InterpLexer::EatNext() {
    lastChar = nextChar;
    if (bufferPos < buffer.size()) {
        nextChar = buffer[bufferPos++];
    } else {
        nextChar = EOF;
    }
}

void InterpLexer::AppendNext() {
    EatNext();
    token.string += lastChar;
}

void InterpLexer::nextToken() {
    token.string = "";
    token.type = TOKEN_UNKOWN;
    while (isspace(nextChar)) EatNext();
    if (isalpha(nextChar) || nextChar == '_') {
        while ((isalnum(nextChar) || nextChar == '_') && nextChar != '.') AppendNext();
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
    else if (isdigit(nextChar)) {
        bool decimalSeen = false;
        while (isdigit(nextChar) || nextChar == '.') {
            if (nextChar == '.') {
                if (!decimalSeen)
                    decimalSeen = true;
                else
                    LOG_ERROR(token.site << "Two decimals found in numeric constant!");
            }
            AppendNext();
        }
        token.type = TOKEN_NUMBER;
    }

        // The Current token is a String Literal
    else if (nextChar == '"') {
        EatNext();
        while (nextChar != '"') {
            AppendNext();
        }
        EatNext();	// Eat the "
        token.type = TOKEN_STRING;
    }

        // COMMENTS
    else if (nextChar == '#') {
        EatNext();	//Eat the '# 'char
        while (nextChar != EOF && nextChar != '\n' && nextChar != '\r')
            EatNext();	//Now we eat the comment body itself
        //We have reached the end of the comment.	If is not the end of the file get the next token
        if (nextChar != EOF) {
            nextToken();
            return;
        }
    }

        // COLON TOKENS
    else if (nextChar == ':') {
        AppendNext();
        if (nextChar == ':') {
            AppendNext();
            token.type = TOKEN_TYPE_DEFINE;
        } else if (nextChar == '=') {
            AppendNext();
            token.type = TOKEN_TYPE_INFER;
        } else if (nextChar == '>') {
            AppendNext();
            token.type = TOKEN_TYPE_RETURN;
        } else {
            token.type = TOKEN_TYPE_DECLARE;
        }
    }

    else if (nextChar == '@') {
        AppendNext();
        token.type = TOKEN_ADDRESS;
    }

    else if (nextChar == '$') {
        AppendNext();
        token.type = TOKEN_VALUE;
    }

    else if (nextChar == '=') {
        AppendNext();
        if (nextChar == '=') {
            AppendNext();
            token.type == TOKEN_LOGIC_EQUAL;
        } else {
            token.type = TOKEN_EQUALS;
        }
    }

    else if (nextChar == '+') {
        AppendNext();
        if (nextChar == '=') {
            AppendNext();
            token.type = TOKEN_ADD_EQUALS;
        } else {
            token.type = TOKEN_ADD;
        }
    }

    else if (nextChar == '-') {
        AppendNext();
        if (nextChar == '=') {
            AppendNext();
            token.type = TOKEN_SUB_EQUALS;
        } else {
            token.type = TOKEN_SUB;
        }
    }

    else if (nextChar == '*') {
        AppendNext();
        if (nextChar == '=') {
            AppendNext();
            token.type = TOKEN_MUL_EQUALS;
        } else {
            token.type = TOKEN_MUL;
        }
    }


    else if (nextChar == '/') {
        AppendNext();
        if (nextChar == '=') {
            AppendNext();
            token.type = TOKEN_DIV_EQUALS;
        } else {
            token.type = TOKEN_DIV;
        }
    }

        // LOGICAL
    else if (nextChar == '~') {
        AppendNext();
        token.type = TOKEN_LOGIC_NOT;
    }


        // REMOVE NEED FOR MODULUS BECAUSE FUCK THAT
        // Mod(5, 2) would be better!
        // else if (nextChar == '%') {
        // 	AppendNext(worker);
        // 	if (nextChar == '=') {
        // 		AppendNext(worker);
        // 		token.type = TOKEN_MOD_EQUALS;
        // 	} else {
        // 		token.type = TOKEN_MOD;
        // 	}
        // }

    else if (nextChar == '.') {
        AppendNext();
        if (nextChar == '.') {
            AppendNext();
            if (nextChar == '.') {
                AppendNext();
                token.type = TOKEN_DOTDOT;
            }
        } else {
            token.type = TOKEN_ACCESS;
        }
    }

        //BRACES, BRACKETS, SUBSCRIPTS
    else if (nextChar == '(') {
        AppendNext();
        token.type = TOKEN_PAREN_OPEN;
    } else if (nextChar == ')') {
        AppendNext();
        token.type = TOKEN_PAREN_CLOSE;
    }	else if (nextChar == '[') {
        AppendNext();
        token.type = TOKEN_ARRAY_OPEN;
    } else if (nextChar == ']') {
        AppendNext();
        token.type = TOKEN_ARRAY_CLOSE;
    } else if (nextChar == '{') {
        AppendNext();
        token.type = TOKEN_BLOCK_OPEN;
        // currentIndentLevel++;
    } else if (nextChar == '}') {
        AppendNext();
        token.type = TOKEN_BLOCK_CLOSE;
        // currentIndentLevel++;
    } else if (nextChar == EOF) {
        //Dont append or ead the EOF
        token.type = TOKEN_EOF;
    } else {
        AppendNext();
        token.type = TOKEN_UNKOWN;
    }
}
#endif


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
    EatNext(worker);
    worker->token.string += worker->lastChar;
}

void EatLine(Worker *worker) {
    while (worker->lastChar != '\n' || worker->lastChar != '\r') {
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

			int indentLevel = 0;
			while (worker->nextChar == '\t' || worker->nextChar == ' ') {
				int spaceCount = 0;
				if (worker->nextChar == ' ') {
					spaceCount++;
					EatNext(worker);
					if (spaceCount > INDENT_SPACE_COUNT) {
                        indentLevel++;
						spaceCount = 0;
					}
				} else {
					EatNext(worker);
                    indentLevel++;
				}
			}

			if (indentLevel > worker->currentIndentLevel) {
				worker->token.string = "";
				worker->token.type = TOKEN_BLOCK_OPEN;;
				worker->token.site.lineNumber = worker->lineNumber;
				worker->token.site.columNumber = worker->colNumber;
				worker->currentIndentLevel = indentLevel;
				return;
			} else if (indentLevel < worker->currentIndentLevel) {
				worker->token.string = "";
				worker->token.type = TOKEN_BLOCK_CLOSE;
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
			if (worker->token.string == "IMPORT") 			    worker->token.type = TOKEN_IMPORT;
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
                if (worker->nextChar == 92) {
                    EatNext(worker);
                    if (worker->nextChar == 'n') worker->nextChar = '\n';
                }
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
				worker->token.type == TOKEN_LOGIC_EQUAL;
			} else {
					worker->token.type = TOKEN_EQUALS;
			}
		}

        else if (worker->nextChar == '>') {
            AppendNext(worker);
            if (worker->nextChar == '=') {
                AppendNext(worker);
                worker->token.type = TOKEN_LOGIC_GREATER_EQAUL;
            } else {
                worker->token.type = TOKEN_LOGIC_GREATER;
            }
        }

        else if (worker->nextChar == '<') {
            AppendNext(worker);
            if (worker->nextChar == '=') {
                AppendNext(worker);
                worker->token.type = TOKEN_LOGIC_LESS_EQUAL;
            } else {
                worker->token.type = TOKEN_LOGIC_LESS;
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
            if (worker->nextChar == '.') {
                AppendNext(worker);
                if (worker->nextChar == '.') {
                    AppendNext(worker);
                    worker->token.type = TOKEN_DOTDOT;
                }
            } else {
                worker->token.type = TOKEN_ACCESS;
            }
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
			worker->token.type = TOKEN_BLOCK_OPEN;
			// currentIndentLevel++;
		} else if (worker->nextChar == '}') {
			AppendNext(worker);
			worker->token.type = TOKEN_BLOCK_CLOSE;
			// currentIndentLevel++;
		} else if (worker->nextChar == EOF) {
			//Dont append or ead the EOF
			worker->token.type = TOKEN_EOF;
		} else {
			AppendNext(worker);
			worker->token.type = TOKEN_UNKOWN;
		}
}
