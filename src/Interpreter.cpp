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


// For now this takes a single package until better packaing is
    printf("%s (", function->name);
    for (U32 i = 0; i < function->args.size(); i++) {
        auto arg = &function->args[i];
        printf("%s : %s", arg->name, arg->type->name);
    }
    printf("insert args here)\n");
}

void RunInterp (Package* package) {
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

    // For now the interpreter will just tell you what things are you wont be able to call things
    // It will do its parsing of whatever you give it and then you can determine infromation about the program
    // by typing in the name of an identifer and it will give you static information about what it actualy is

    bool isRunning = true;
    std::string input;
    while (isRunning) {
		printf("> ");
        std::cin >> input;
        auto node = FindNodeWithIdent(&package->globalBlock, input);
        if (node == nullptr) {
            printf("That doesnt name anything");
        }

        switch (node->nodeType) {
            case AST_FUNCTION: {
                auto funcSet = (ASTFunctionSet *) node;
                printf("Found %d functions matching: %s\n", funcSet->functions.size(), input.c_str());
                for (U32 i = 0; i < funcSet->functions.size(); i++) {
                    Print((ASTFunction *) funcSet->functions[i]);
                }
            } break;

            default:
                printf("Dont know what that is yet but it exists!");
        }
	}
}
