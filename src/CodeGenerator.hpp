/*
 * CodeGenerator.hpp
 *
 *  Created on: Jun 18, 2015
 *      Author: torin
 */

#ifndef _CODEGENERATOR_HPP
#define _CODEGENERATOR_HPP

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

	llvm::Value* Codegen(float value);
	llvm::Value* Codegen(ASTExpression* expression);
	llvm::Value* Codegen(ASTVariable* variable);
	llvm::Function* Codegen(ASTPrototype* proto);
	llvm::Function* Codegen(ASTFunction* function);

private:
	llvm::Module* module;
	llvm::IRBuilder<>* builder;
};


#endif /* CODEGENERATOR_HPP_ */
