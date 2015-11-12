#include "ParserCommon.hpp"

ASTExpression* ParseExpr(MemoryArena* arena, Lexer* lex) {
    auto parsePrimary = [](MemoryArena* arena, Lexer* lex) -> ASTExpression* {
        switch (lex->token.type) {
            case TOKEN_TRUE: {
                lex->nextToken();
                return CreateIntegerLiteral(arena, 1);
            }
            case TOKEN_FALSE: {
                lex->nextToken();
                return CreateIntegerLiteral(arena, 0);
            }
            case TOKEN_STRING: {
                auto str = CreateStringLiteral (arena, lex->token.string);
                lex->nextToken();
                return str;
            }

            case TOKEN_NUMBER: {
                LOG_VERBOSE("Parsing a numberExpression!");
                auto dotPos = lex->token.string.find(".");
                bool isFloat = dotPos == std::string::npos ? false : true;
                if (isFloat) {
                    if(lex->token.string.substr(dotPos + 1).find(".") != std::string::npos) {
                        // ReportError(worker, worker->token.site, "Floating Point value contains two decimal points!");
                    }
                    auto value = std::stof(lex->token.string);
                    auto result = CreateFloatLiteral(arena, value);
                    lex->nextToken();
                    return result;
                } else {
                    auto value = std::stoi(lex->token.string);
                    auto result = CreateIntegerLiteral(arena , value);
                    lex->nextToken();
                    return result;
                }
            }

        }
        return nullptr;
    };

    std::function<ASTExpression*(MemoryArena*, Lexer*, ASTExpression*, int)> parseBinary
            = [&parsePrimary, &parseBinary](MemoryArena* arena, Lexer* lex, ASTExpression* lhs, int expressionPrec) -> ASTExpression* {
        assert(lhs != nullptr);
        while (true) {

            auto tokenPrec = GetTokenPrecedence(lex->token);
            if (tokenPrec < 0) return lhs;	// Bail if there is no binop

            auto binopToken = lex->token;
            lex->nextToken();

            // We have a binop lets see what is on the other side!
            ASTExpression* rhs = parsePrimary(arena, lex);
            if (rhs == nullptr) {
                //ReportError(worker, binopToken.site, "Could not parse primary expression to the right of binary opperator '" + binopToken.string	+ "'");
                return nullptr;
            }

            auto nextPrec = GetTokenPrecedence (lex->token);
            if (tokenPrec < nextPrec) {
                rhs = parseBinary(arena, lex, rhs, tokenPrec + 1);
                if (rhs == nullptr) {
                    LOG_ERROR("Could not parse recursive rhsParsing!");
                    return nullptr;
                }
            }

            lhs = (ASTExpression*)CreateBinaryOperation(arena, TokenToOperation(binopToken), lhs, rhs);
        }	 // Goes back to the while loop
    };


    auto lhs = parsePrimary(arena, lex);
    if (lhs == nullptr) return nullptr;
    return parseBinary(arena, lex, lhs, 0);
}