#pragma once

#include "llvm/Support/FormattedStream.h"
#include "llvm/Support/raw_os_ostream.h"

#include "llvm/IR/Constant.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Verifier.h"

#include "AST.hpp"
#include "Common.hpp"

class CodeGenerator {
public:
	CodeGenerator(llvm::Module* module);
	~CodeGenerator();

	llvm::Value* Codegen(ASTNode* node);
	llvm::Value* Codegen(ASTBinaryOperation* binop);
	llvm::Value* Codegen(ASTReturn* retVal);
	llvm::Value* Codegen(ASTVariable* var);
	llvm::Value* Codegen(ASTMutation* mut);
	llvm::Value* Codegen(ASTCall* call);
	llvm::Value* Codegen(ASTIfStatement* ifStatment, llvm::BasicBlock* mergeBlock, llvm::Function* function);
	llvm::Value* Codegen(ASTIntegerLiteral* intLiteral);
	llvm::Value* Codegen(ASTFloatLiteral* floatLiteral);
	llvm::Function* Codegen(ASTFunction* function);
	llvm::Value* Codegen(ASTBlock* block);

private:
	llvm::Module* module;
	llvm::IRBuilder<>* builder;
};
