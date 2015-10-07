#pragma once

struct InterpLexer {
public:
    Token token;
    std::string buffer;
    void nextToken();
    void begin();
    void end(); // clear out the buffer
private:
    void EatNext();
    void AppendNext();
    U32 bufferPos;
    char lastChar = 0, nextChar = 0;
};
