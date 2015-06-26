/*
 * CodeGenerator.cpp
 *
 *  Created on: Jun 18, 2015
 *      Author: torin
 */

#include "CodeGenerator.hpp"

CodeGenerator::CodeGenerator(llvm::Module* module) {
	this->module = module;
	builder = new llvm::IRBuilder<>(module->getContext());
}

CodeGenerator::~CodeGenerator() {
	delete builder;
}

llvm::Value* CodeGenerator::Codegen(ASTNode* node) {
	if (typeid(*node) == typeid(ASTVariable))
		return Codegen((ASTVariable*) node);
	if (typeid(*node) == typeid(ASTFunction))
		return Codegen((ASTFunction*) node);
	if (typeid(*node) == typeid(ASTCall))
		return Codegen((ASTCall*) node);

	LOG_ERROR("UNHANDLED CODEGEN OF ASTNODE");
	return nullptr;
}

llvm::Value* CodeGenerator::Codegen(ASTExpression* expression) {
	return Codegen((ASTNode*) expression);
}

llvm::Value* CodeGenerator::Codegen(ASTVariable* variable) {
	LOG_VERBOSE("Codegen Variable");
	return llvm::ConstantInt::get(llvm::Type::getInt32Ty(module->getContext()), 1);
	//return nullptr;
}

llvm::Value* CodeGenerator::Codegen(float value) {
	return llvm::ConstantFP::get(llvm::Type::getFloatTy(module->getContext()), value);
}

llvm::Value* CodeGenerator::Codegen(ASTCall* call) {
	llvm::Function* function = module->getFunction(call->functionName);
	if (function == 0) {
		LOG_ERROR("Call to undefined function(" << call->functionName << ")");
		return nullptr;
	}

	if (call->args.size() != function->arg_size()) {
		LOG_ERROR("Function Call contains incorrect number of arguments!");
		return nullptr;
	}

	std::vector<llvm::Value*> argsV;
	for (uint32 i = 0, e = function->arg_size(); i != e; i++) {
		argsV.push_back(Codegen(call->args[i]));
		if (argsV.back() == 0)
			return nullptr;
	}

	return builder->CreateCall(function, argsV, "calltmp");
}

llvm::Function* CodeGenerator::Codegen(ASTFunction* function) {
	LOG_VERBOSE("Codgen Function");
	llvm::FunctionType* funcType = llvm::FunctionType::get(function->returnType->llvmType, false);
	llvm::Function::LinkageTypes linkage = (function->body.size() == 0) ? llvm::Function::ExternalLinkage : llvm::Function::ExternalLinkage;
	llvm::Function* llvmFunc = llvm::Function::Create(funcType, linkage, function->name, module);

	if (function->body.size() > 0) {
		llvm::BasicBlock* block = llvm::BasicBlock::Create(module->getContext(), "entry", llvmFunc);
		builder->SetInsertPoint(block);

		//Create a new block insider this function and
		//Set the IRBuilders insertion point to the block

		for (ASTNode* node : function->body) {
			Codegen(node);
		}

		llvm::Value* returnValue = llvm::ConstantInt::get(function->returnType->llvmType, 1);
		if (returnValue) {
			builder->CreateRet(returnValue);
			llvm::raw_os_ostream* stream = new llvm::raw_os_ostream(std::cout);
			llvm::verifyModule(*module, stream);
			return llvmFunc;
		}
	}

	return llvmFunc;

	//There was an error reading the body of the function
	//Remove the function
//	function->eraseFromParent();
//	LOG_ERROR("Error parsing body of function");
	return nullptr;
}
