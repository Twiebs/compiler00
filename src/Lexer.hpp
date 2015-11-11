#pragma once

enum TokenType {
    TOKEN_UNKOWN,
    TOKEN_IMPORT,
    TOKEN_FOREIGN,
    TOKEN_IDENTIFIER,

    TOKEN_TYPE_DEFINE,
    TOKEN_TYPE_DECLARE,
    TOKEN_TYPE_INFER,
    TOKEN_TYPE_RETURN,

    TOKEN_STRUCT,
    TOKEN_ACCESS,	// TODO MemberAccess to be clearer?

    TOKEN_ADDRESS,
    TOKEN_VALUE,

    // Binops
    TOKEN_CONSTRUCT,
    TOKEN_ADD,
    TOKEN_SUB,
    TOKEN_MUL,
    TOKEN_DIV,

    TOKEN_LOGIC_NOT,

    TOKEN_LOGIC_OR,
    TOKEN_LOGIC_AND,
    TOKEN_LOGIC_EQUAL,
    TOKEN_LOGIC_GREATER,
    TOKEN_LOGIC_LESS,
    TOKEN_LOGIC_GREATER_EQAUL,
    TOKEN_LOGIC_LESS_EQUAL,

    TOKEN_EQUALS,
    TOKEN_ADD_EQUALS,
    TOKEN_SUB_EQUALS,
    TOKEN_MUL_EQUALS,
    TOKEN_DIV_EQUALS,

    // Keywords
            TOKEN_IF,
    TOKEN_ELSE,
    TOKEN_ITER,
    TOKEN_TO,
    TOKEN_RETURN,

    // Literals
            TOKEN_TRUE,
    TOKEN_FALSE,

    // TODO we check if the token has a dot in it during the lex phase
    // then we check if it has a dot again in the parExpr phase.
    // Just return a different token it will be cheaper
            TOKEN_NUMBER,
    TOKEN_STRING,

    TOKEN_DOTDOT,

    TOKEN_PAREN_OPEN,
    TOKEN_PAREN_CLOSE,
    TOKEN_BLOCK_OPEN,
    TOKEN_BLOCK_CLOSE,
    TOKEN_ARRAY_OPEN,
    TOKEN_ARRAY_CLOSE,
    TOKEN_END_OF_BUFFER,
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

struct Token {
    FileSite site;
    TokenType type;
    std::string string;
};

class Lexer {
public:
    Token token;

    void nextToken();
    void eatLine();

    void SetBuffer(const char* buffer);
private:

    const char* currentChar;
    U32 columnNumber, lineNumber;
    U32 currentIndentLevel;

    void AppendNext();
    void EatNext();
};