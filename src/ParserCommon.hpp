#ifndef LLVMLANG_PARSERCOMMON_HPP
#define LLVMLANG_PARSERCOMMON_HPP

#include "Lexer.hpp"
#include "AST.hpp"

ASTExpression* ParseExpr(MemoryArena* arena, Lexer* lex);

#endif //LLVMLANG_PARSERCOMMON_HPP
