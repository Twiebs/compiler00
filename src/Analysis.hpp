#ifndef LLVMLANG_ANALYSIS_HPP
#define LLVMLANG_ANALYSIS_HPP

void AnalyzeStatement (ASTNode* node, ASTBlock* currentBlock);
void AnalyzeExpr (ASTExpression* expr, ASTBlock* currentBlock);

S8 GetAbsoulteIndirectionLevelForExpression(ASTExpression* expr);

#endif //LLVMLANG_ANALYSIS_HPP
