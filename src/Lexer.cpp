#define USE_INDENT_BLOCK
#define INDENT_SPACE_COUNT 2

#include "Common.hpp"
#include "Build.hpp"

#include "Lexer.hpp"

void Lexer::SetBuffer(const char* buffer) {
    this->currentChar = buffer;
    columnNumber = 0;
    lineNumber = 0;
}

void Lexer::AppendNext() {
    token.string += *currentChar;
    EatNext();
}

//    worker->lastChar = worker->nextChar;
//    worker->nextChar = getc(worker->file);
//    worker->colNumber++;
//    if (worker->lastChar == '\n') {
//        worker->lineNumber++;
//        worker->colNumber = 1;
//    }

void Lexer::EatNext() {
    if (*currentChar == '\n') {
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

static inline bool IsNewline (const char* current) {
    auto result = *current == '\n';
    return result;
}

void Lexer::nextToken() {
#ifdef USE_INDENT_BLOCK
    if (IsNewline(currentChar)) {
        EatNext();
        if (IsNewline(currentChar)) {
            nextToken();
            return;
        }

        int indentLevel = 0;
        while (*currentChar == '\t' || *currentChar == ' ') {
            int spaceCount = 0;
            if (*currentChar == ' ') {
                spaceCount++;
                EatNext();
                if (spaceCount > INDENT_SPACE_COUNT) {
                    indentLevel++;
                    spaceCount = 0;
                }
            } else {
                EatNext();
                indentLevel++;
            }
        }

        if (indentLevel > currentIndentLevel) {
            token.string = "";
            token.type = TOKEN_BLOCK_OPEN;;
            token.site.lineNumber = lineNumber;
            token.site.columNumber = columnNumber;
            currentIndentLevel = indentLevel;
            return;
        } else if (indentLevel < currentIndentLevel) {
            token.string = "";
            token.type = TOKEN_BLOCK_CLOSE;
            token.site.lineNumber = lineNumber;
            token.site.columNumber = columnNumber;
            currentIndentLevel = indentLevel;
            return;
        }
    }
#endif

    token.string = "";
    token.type = TOKEN_UNKOWN;
    while (isspace(*currentChar)) EatNext();
    if (isalpha(*currentChar) || *currentChar== '_') {
        while ((isalnum(*currentChar) || *currentChar == '_') && *currentChar!= '.') AppendNext();
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
            AppendNext();
        }
        token.type = TOKEN_NUMBER;
    }

        // The Current token is a String Literal
    else if (*currentChar== '"') {
        EatNext();
        while (*currentChar != '"') {
            AppendNext();
        }
        EatNext();	// Eat the "
        token.type = TOKEN_STRING;
    }

        // COMMENTS
    else if (*currentChar == '#') {
        EatNext();	//Eat the '# 'char
        while (*currentChar != EOF && *currentChar != '\n' && *currentChar != '\r')
            EatNext();	//Now we eat the comment body itself
        //We have reached the end of the comment.	If is not the end of the file get the next token
        if (*currentChar != EOF) {
            nextToken();
            return;
        }
    }

        // COLON TOKENS
    else if (*currentChar== ':') {
        AppendNext();
        if (*currentChar== ':') {
            AppendNext();
            token.type = TOKEN_TYPE_DEFINE;
        } else if (*currentChar == '=') {
            AppendNext();
            token.type = TOKEN_TYPE_INFER;
        } else {
            token.type = TOKEN_TYPE_DECLARE;
        }
    }

    else if (*currentChar == '@') {
        AppendNext();
        token.type = TOKEN_ADDRESS;
    }

    else if (*currentChar == '$') {
        AppendNext();
        token.type = TOKEN_VALUE;
    }

    else if (*currentChar == '=') {
        AppendNext();
        if (*currentChar == '=') {
            AppendNext();
            token.type == TOKEN_LOGIC_EQUAL;
        } else {
            token.type = TOKEN_EQUALS;
        }
    }

    else if (*currentChar == '+') {
        AppendNext();
        if (*currentChar == '=') {
            AppendNext();
            token.type = TOKEN_ADD_EQUALS;
        } else {
            token.type = TOKEN_ADD;
        }
    }

    else if (*currentChar == '-') {
        AppendNext();
        if (*currentChar == '=') {
            AppendNext();
            token.type = TOKEN_SUB_EQUALS;
        } else {
            token.type = TOKEN_SUB;
        }
    }

    else if (*currentChar == '*') {
        AppendNext();
        if (*currentChar == '=') {
            AppendNext();
            token.type = TOKEN_MUL_EQUALS;
        } else {
            token.type = TOKEN_MUL;
        }
    }


    else if (*currentChar == '/') {
        AppendNext();
        if (*currentChar == '=') {
            AppendNext();
            token.type = TOKEN_DIV_EQUALS;
        } else {
            token.type = TOKEN_DIV;
        }
    }

        // LOGICAL
    else if (*currentChar == '~') {
        AppendNext();
        token.type = TOKEN_LOGIC_NOT;
    }

    else if (*currentChar == '>') {
        AppendNext();
        if (*currentChar == '>') {
            AppendNext();
            token.type = TOKEN_TYPE_RETURN;
        } else if (*currentChar == '=') {
            AppendNext();
            token.type = TOKEN_LOGIC_GREATER_EQAUL;
        } else {
            token.type = TOKEN_LOGIC_GREATER;
        }
    }

    else if (*currentChar == '<') {
        AppendNext();
        if (*currentChar == '<') {
            AppendNext();
            token.type = TOKEN_CONSTRUCT;
        } else if (*currentChar == '=') {
            AppendNext();
            token.type = TOKEN_LOGIC_LESS_EQUAL;
        } else {
            token.type = TOKEN_LOGIC_LESS;
        }
    }

    else if (*currentChar == '.') {
        AppendNext();
        if (*currentChar == '.') {
            AppendNext();
            if (*currentChar == '.') {
                AppendNext();
                token.type = TOKEN_DOTDOT;
            }
        } else {
            token.type = TOKEN_ACCESS;
        }
    }

        //BRACES, BRACKETS, SUBSCRIPTS
    else if (*currentChar == '(') {
        AppendNext();
        token.type = TOKEN_PAREN_OPEN;
    } else if (*currentChar == ')') {
        AppendNext();
        token.type = TOKEN_PAREN_CLOSE;
    }	else if (*currentChar == '[') {
        AppendNext();
        token.type = TOKEN_ARRAY_OPEN;
    } else if (*currentChar == ']') {
        AppendNext();
        token.type = TOKEN_ARRAY_CLOSE;
    } else if (*currentChar == '{') {
        AppendNext();
        token.type = TOKEN_BLOCK_OPEN;
        // currentIndentLevel++;
    } else if (*currentChar == '}') {
        AppendNext();
        token.type = TOKEN_BLOCK_CLOSE;
        // currentIndentLevel++;
    } else if (*currentChar == EOF || *currentChar == 0) {
        //Dont append or ead the EOF or buffer
        token.type = TOKEN_END_OF_BUFFER;
    } else {
        AppendNext();
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
