#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/GenericValue.h"
#include "llvm/ExecutionEngine/MCJIT.h"

#include "llvm/Support/TargetSelect.h"

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"

#include "Common.hpp"
#include "Build.hpp"

extern "C" void InterpTest() {
	printf("Interpreter Test was sucuessfull\n");
	printf("Hell yeah!!!  Calling a native function in bitcode!\n");
}

// The interpreter must be run within a worker
// No idea if this is a good idea or not but we
// Need to allocate the ASTNodes and stuff
// So Who knows how this will go
void RunInterp () {
	llvm::InitializeAllTargets();
	llvm::InitializeAllTargetMCs();

	auto memoryManager = std::make_unique<llvm::SectionMemoryManager>();
	auto unique_module = std::make_unique<llvm::Module>("Bang Interpreter", llvm::getGlobalContext());
	auto module = unique_module.get();

	std::vector<llvm::Type*> args;
    args.push_back(llvm::PointerType::getInt8PtrTy(llvm::getGlobalContext(), 0));
	auto funcType = llvm::FunctionType::get(llvm::Type::getVoidTy(llvm::getGlobalContext()), args, false);
	auto testFunc = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, "Build", module);

	std::string errorStr;
	auto engine = llvm::EngineBuilder(std::move(unique_module)).setMCJITMemoryManager(std::move(memoryManager)).setErrorStr(&errorStr).create();
	if (engine == nullptr) {
		LOG_ERROR("Failed to create ExecutionEngine: " << errorStr);
	}


    std::string str;
    bool isRunning = true;
	while (isRunning) {
		printf("> ");
        std::cin >> str;
        if (!str.compare("Build")) {
            std::vector<llvm::GenericValue> funcargs;
            engine->runFunction(testFunc, funcargs);
        }
	}
}
