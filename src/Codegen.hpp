#pragma once

#include "llvm/IR/IRBuilder.h"
#include "AST.hpp"
#include "Build.hpp"

// Is it possible to compleatly segergate Statements and Expressions
// From eachother?
// Why do we actualy need an ASTNode?  Thats such an abstract thing for no reason
// What purpose does it actualy serve?
// We could write an ASTCast Macro that does some hacks to cast it to the right type


void Codegen(Package* package, const BuildContext& context);

// Just Pass The Builder?
// Or... Just Create a global builder in the CPP file
// that actualy sounds like a remotly sane idea...
// Also why should these functions now be in the hpp file if were just going to call Codegen on packages!
llvm::Value* Codegen(ASTNode* node, const BuildContext& context);
llvm::Value* Codegen(ASTBlock* block, const BuildContext& context);
llvm::Value* Codegen(ASTReturn* retVal, const BuildContext& context);
llvm::Value* Codegen(ASTIfStatement* ifStatment, llvm::BasicBlock* mergeBlock, llvm::Function* function, const BuildContext& context);
llvm::Function* Codegen(ASTFunction* function, const BuildContext& context);

llvm::Value* Codegen(ASTBinaryOperation* binop, const BuildContext& context);
llvm::Value* Codegen(ASTVariable* var, const BuildContext& context);
llvm::Value* Codegen(ASTMutation* mut, const BuildContext& context);
llvm::Value* Codegen(ASTCall* call, const BuildContext& context);
llvm::Value* Codegen(ASTIntegerLiteral* intLiteral, const BuildContext& context);
llvm::Value* Codegen(ASTFloatLiteral* floatLiteral, const BuildContext& context);


