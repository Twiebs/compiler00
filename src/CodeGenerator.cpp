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

llvm::Value* CodeGenerator::Codegen(ASTVariable* variable) {
	LOG_VERBOSE("Codegen Variable");
	return llvm::ConstantInt::get(llvm::Type::getInt32Ty(module->getContext()), 1);
	//return nullptr;
}

llvm::Value* CodeGenerator::Codegen(ASTExpression* expression) {
	if(typeid(*expression) == typeid(ASTVariable)) {
		return Codegen((ASTVariable*) expression);
	}

	LOG_ERROR("UNHANDLED CODEGEN!");
	return nullptr;
}

llvm::Value* CodeGenerator::Codegen(float value) {
	return llvm::ConstantFP::get(llvm::Type::getFloatTy(module->getContext()), value);
}

llvm::Value* CodeGenerator::Codegen(ASTCall* call) {
	llvm::Function* function = module->getFunction(call->functionName);
	if(function == 0) {
		LOG_ERROR("Call to undefined function(" << call->functionName << ")");
		return nullptr;
	}

	if(call->args.size() != function->arg_size()) {
		LOG_ERROR("Function Call contains incorrect number of arguments!");
		return nullptr;
	}

	std::vector<llvm::Value*> argsV;
	for(uint32 i = 0, e = function->arg_size(); i != e; i++) {
		argsV.push_back(Codegen(call->args[i]));
		if(argsV.back() == 0) return nullptr;
	}

	return builder->CreateCall(function, argsV, "calltmp");
}


//Generates IR for ASTFunctions
//Does not require a prototype
//TODO use #foreign to set external or internal linkage
//Maybe thats not the best idea..
//perhaps internal and external could be used
llvm::Function* CodeGenerator::Codegen(ASTPrototype* prototype) {
	LOG_VERBOSE("Codegen Prototype");
	llvm::FunctionType* funcType = llvm::FunctionType::get(llvm::Type::getInt32Ty(module->getContext()), false);
	llvm::Function* func = llvm::Function::Create(funcType, prototype->isForegin ? llvm::Function::ExternalLinkage : llvm::Function::ExternalLinkage, prototype->name, module);

	if (func->getName() != prototype->name) {
		func->eraseFromParent();
		func = module->getFunction(prototype->name);
	}

	//if(!llvm::verifyFunction(*func)) {
	//	LOG_ERROR("Function verification failed!");
	//}
	return func;
}

llvm::Function* CodeGenerator::Codegen(ASTFunction* functionAST) {
	LOG_VERBOSE("Codgen Function");
	llvm::Function* function = Codegen(functionAST->proto);
	if(function == nullptr)
		return nullptr;

	//Create a new block insider this function and
	//Set the IRBuilders insertion point to the block
	llvm::BasicBlock* block = llvm::BasicBlock::Create(module->getContext(), "entry", function);
	builder->SetInsertPoint(block);

	llvm::Value* returnValue = Codegen(functionAST->body);
	if(returnValue) {
		builder->CreateRet(returnValue);
		llvm::verifyModule(*module);
		return function;
	}

	//There was an error reading the body of the function
	//Remove the function
	function->eraseFromParent();
	LOG_ERROR("Error parsing body of function");
	return nullptr;
}
