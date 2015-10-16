#pragma once

llvm::Value* CodegenExpr (ASTNode* node);
llvm::IRBuilder<>* GetGlobalBuilderHack();