#pragma once

void AnalyzeStatement (ASTNode* node, ASTBlock* currentBlock);
void AnalyzeExpr (ASTExpression* expr, ASTBlock* currentBlock);

S8 GetAbsoulteIndirectionLevelForExpression(ASTExpression* expr);