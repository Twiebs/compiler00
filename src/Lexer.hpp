#pragma once

struct InterpLexer {
public:
    Token token;
    std::string buffer;
    void nextToken();
    void setBuffer(const std::string& buffer);
    void begin();
private:
    void EatNext();
    void AppendNext();
    U32 bufferPos;
    char lastChar = 0, nextChar = 0;
};
