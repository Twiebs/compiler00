/*
 * CodeGenerator.hpp
 *
 *  Created on: Jun 18, 2015
 *      Author: torin
 */

#ifndef _CODEGENERATOR_HPP
#define _CODEGENERATOR_HPP

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

	llvm::Value* Codegen(AST::Node* node);
	llvm::Value* Codegen(AST::BinaryOperation* binop);
	llvm::Value* Codegen(AST::ReturnValue* retVal);
	llvm::Value* Codegen(AST::Variable* var);
	llvm::Value* Codegen(AST::VariableMutation* mut);
	llvm::Value* Codegen(AST::Call* call);
	llvm::Value* Codegen(AST::IfStatement* ifStatment, llvm::BasicBlock* mergeBlock, llvm::Function* function);
	llvm::Value* Codegen(AST::IntegerLiteral* intLiteral);
	llvm::Value* Codegen(AST::FloatLiteral* floatLiteral);
	llvm::Function* Codegen(AST::Function* function);
	llvm::Value* Codegen(AST::Block* block);

private:
	llvm::Module* module;
	llvm::IRBuilder<>* builder;
};


#endif /* CODEGENERATOR_HPP_ */
