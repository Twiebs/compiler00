#pragma once

#include <string>

#include "AST.hpp"
#include "Lexer.hpp"

class Parser {
    std::string filename;
    Lexer lex;
    ASTBlock* currentBlock;

    void ParseTopLevelStatement();
    ASTNode* ParseStatement();

public:
    void ParseFile(const std::string& root_directory, const std::string& filename);

};