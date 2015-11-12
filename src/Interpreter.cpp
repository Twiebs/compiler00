#include <llvm/IR/IRBuilder.h>
#include <llvm/Support/FormattedStream.h>
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/GenericValue.h"
#include "llvm/ExecutionEngine/MCJIT.h"
#include "llvm/ExecutionEngine/Interpreter.h"

#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/raw_os_ostream.h"

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"

#include "Common.hpp"
#include "Build.hpp"
#include "Analysis.hpp"
#include "Lexer.hpp"
#include "Codegen.hpp"

#include "ParserCommon.hpp"

extern "C" void InterpTest() {
	printf("Interpreter Test was sucuessfull\n");
	printf("Hell yeah!!!  Calling a native function in bitcode!\n");
}

// For now this takes a single package until better packaing is
// TODO DOUBLE POINTER
void Print (ASTFunction* function) {
    printf("%s :: (", function->name);
    for (U32 i = 0; i < function->args.size(); i++) {
        ASTVariable* arg = function->args[i];
        if (arg->isPointer) {
            printf("%s : @%s", arg->name, arg->type->name);
        } else {
            printf("%s : %s", arg->name, arg->type->name);
        }
    }
    printf(") ");
    if (function->returnType != nullptr) {
        printf(">> %s", function->returnType->name);
    }

    // TODO functions are designated foregin if they do not contain a body
    // This may be foolish it would probably be better to include a flag that sets it as foreign
    // because it may be a good idea to allow function declerations without definitions
    // however, it may prove to be completly unessecary with the other faciltys in the language
    if (function->members.size() == 0) {
        printf(" FOREIGN");
    }

    printf("\n");
}

// TODO there is no way to declare double pointers in the language!

// TODO @Incomplete does not understand double pointers because they are currently not implemented
void Print (ASTStruct* structDefn) {
    printf("%s :: STRUCT\n", structDefn->name);
    for (U32 i = 0; i < structDefn->memberCount; i++) {
        auto structMember = &structDefn->members[i];
        if (structMember->isPointer) {
            printf("\t%s : @%s\n", structMember->name, structMember->type->name);
        } else {
            printf("\t%s : %s\n", structMember->name, structMember->type->name);
        }
    }
	printf("\n");
}

internal void PrintDefn (ASTNode* node) {
	switch (node->nodeType) {
	case AST_FUNCTION: Print((ASTFunction*)node); break;
	case AST_STRUCT: Print((ASTStruct*)node); break;
	default: assert(false); break;
	}
}


internal void Print (ASTIntegerLiteral* literal) {
	printf("%d : %s\n", literal->value, literal->type->name);
}

internal void Print (ASTStringLiteral* literal) {
	printf("'%s' : @%s\n", literal->value, literal->type->name);
}

internal void PrintExpr (ASTExpression* expr) {
	switch (expr->nodeType) {
		case AST_INTEGER_LITERAL:
			Print((ASTIntegerLiteral*)expr);
			break;
		case AST_STRING_LITERAL:
			Print((ASTStringLiteral*)expr);
			break;
	}
}


global_variable Package* global_package;
// HACK this is a quick hack for fun
extern "C" void ListAll() {
	Package* package = global_package;
	for (U32 i = 0; i < package->rootBlock.members.size(); i++) {
		auto node = package->rootBlock.members[i];
		PrintDefn(node);
	}
}

static llvm::Function* GenerateTestFunction (llvm::Module* module) {
    llvm::IRBuilder<> builder(llvm::getGlobalContext());
    auto functionType = llvm::FunctionType::get(llvm::Type::getFloatTy(llvm::getGlobalContext()), false);
    auto function = llvm::Function::Create(functionType, llvm::Function::LinkageTypes::ExternalLinkage, "ExampleFunction", module);

    auto entryBlock = llvm::BasicBlock::Create(llvm::getGlobalContext(), "entry", function);
    builder.SetInsertPoint(entryBlock);
    builder.CreateRet(llvm::ConstantFP::get(llvm::Type::getFloatTy(llvm::getGlobalContext()), 9));
    return function;
}

void RunInterpTest() {
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    LLVMLinkInMCJIT();

    auto memoryManager = std::make_unique<llvm::SectionMemoryManager>();
    auto unique_module = std::make_unique<llvm::Module>("Bang Interpreter", llvm::getGlobalContext());
    auto module = unique_module.get();
    auto testFunction = GenerateTestFunction(module);
    testFunction->dump();

    std::string errorStr;
    // auto engine = llvm::EngineBuilder(std::move(unique_module)).setMCJITMemoryManager(std::move(memoryManager)).setErrorStr(&errorStr).create();
    auto engine = llvm::EngineBuilder(std::move(unique_module)).setEngineKind(llvm::EngineKind::Interpreter).setErrorStr(&errorStr).create();
    if (engine == nullptr) LOG_ERROR("Failed to create ExecutionEngine: " << errorStr);

    assert(testFunction);
    std::vector<llvm::GenericValue> testArgs;
    auto result = engine->runFunction(testFunction, testArgs);
    float returnValue = result.FloatVal;
    printf("test function returned: %f", returnValue);
}



void RunInterp (Package* package) {
	global_package = package;
	llvm::InitializeAllTargets();
	llvm::InitializeAllTargetMCs();

	auto memoryManager = std::make_unique<llvm::SectionMemoryManager>();
	auto unique_module = std::make_unique<llvm::Module>("Bang Interpreter", llvm::getGlobalContext());
	auto module = unique_module.get();
   // auto testFunction = GenerateTestFunction(module);

//  llvm::IRBuilder<> builder(llvm::getGlobalContext());
//	auto anonFunctionType = llvm::FunctionType::get(llvm::Type::getFloatTy(llvm::getGlobalContext()), false);
//	auto function = llvm::Function::Create(functionType, llvm::Function::LinkageTypes::ExternalLinkage, "REPLAnon", module);

	std::vector<llvm::Type*> args;
    args.push_back(llvm::PointerType::getInt8PtrTy(llvm::getGlobalContext(), 0));
	auto funcType = llvm::FunctionType::get(llvm::Type::getVoidTy(llvm::getGlobalContext()), args, false);
	auto testFunc = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, "Build", module);

	std::string errorStr;
	// auto engine = llvm::EngineBuilder(std::move(unique_module)).setMCJITMemoryManager(std::move(memoryManager)).setErrorStr(&errorStr).create();
    auto engine = llvm::EngineBuilder(std::move(unique_module)).setEngineKind(llvm::EngineKind::Interpreter).setErrorStr(&errorStr).create();
    if (engine == nullptr) LOG_ERROR("Failed to create ExecutionEngine: " << errorStr);

    // For now the interpreter will just tell you what things are you wont be able to call things
    // It will do its parsing of whatever you give it and then you can determine infromation about the program
    // by typing in the name of an identifer and it will give you static information about what it actualy is

   // package->llvmModule->dump();
    bool isRunning = true;
    std::string inputBuffer;
    // InterpLexer lex;
    Lexer lex;

	MemoryArena arena;
	arena.memory = malloc(ARENA_BLOCK_SIZE);

    while (isRunning) {
        // lex.end(); // clears the buffer from the command line
        printf("> ");
        std::cin >> inputBuffer;
        lex.SetBuffer(inputBuffer.c_str());
        lex.nextToken();

//        char lastChar = getchar();
//        while (lastChar != '\n') {
//            lex.buffer += lastChar;
//            lastChar = getchar();
//        }

//        lex.begin();
//        lex.nextToken();

		static auto REPLEvaluate = [&engine, &module](ASTExpression* expr) {
			if (!engine->removeModule(module)) assert(false);
            auto anonFunctionType = llvm::FunctionType::get((llvm::Type*)expr->type->llvmType, false);
			auto anonFunction = llvm::Function::Create(anonFunctionType, llvm::Function::LinkageTypes::InternalLinkage, "anon", module);
			auto entryBlock = llvm::BasicBlock::Create(llvm::getGlobalContext(), "entry", anonFunction);
            auto builder = GetGlobalBuilderHack();
            builder->SetInsertPoint(entryBlock);
            auto exprValue = CodegenExpr(expr);
			assert(exprValue != nullptr);
			// auto exprLoad = builder->CreateLoad(exprValue, "returnLoad");
			auto returnValue = builder->CreateRet(exprValue);

			module->dump();
			engine->addModule(std::unique_ptr<llvm::Module>(module));
			std::vector<llvm::GenericValue> args;
			auto result = engine->runFunction(anonFunction, args);

			if (isSignedInteger(expr->type)) {
				auto intValue = result.IntVal.getSExtValue();
				printf("The result is %d ", intValue);
			} else if (isFloatingPoint(expr->type)) {
				auto floatValue = result.FloatVal;
				printf("The result is %f", floatValue);
			} else {
			}

		};

		static auto REPLProc = [](MemoryArena* arena, Lexer* lex) -> bool {
			auto expr = ParseExpr(arena, lex);
			if (expr == nullptr) return false;
			AnalyzeExpr(expr, nullptr);
			REPLEvaluate(expr);
			PrintExpr(expr);
			return true;
		};

		if (REPLProc(&arena, &lex)) continue;

        if (lex.token.type == TOKEN_IDENTIFIER) {
            Token identToken = lex.token;
            lex.nextToken();
            if (lex.token.type == TOKEN_PAREN_OPEN) {
                lex.nextToken();
				// TODO parse the arguments correctly here!
				lex.nextToken();

                auto node = FindNodeWithIdent(&package->rootBlock, identToken.string);
                if (node == nullptr) {
                    printf("could not find a function named %s\n", identToken.string.c_str());
                } else {
                    auto funcSet = (ASTFunctionSet*)node;
                    auto function = funcSet->functions[0];
                    std::vector<llvm::GenericValue> args;
                    engine->runFunction((llvm::Function *) function->llvmFunction, args);
                }
            } else {
                auto node = FindNodeWithIdent(&package->rootBlock, identToken.string);
                bool listIR = false;
                if (lex.token.type != TOKEN_END_OF_BUFFER) {
                    if (lex.token.string == "IR") {
                        listIR = true;
                    }
                }

                if (node == nullptr) {
                    printf("That doesnt name anything\n");
                    continue;
                }

                switch (node->nodeType) {
                    case AST_FUNCTION: {
                        auto funcSet = (ASTFunctionSet *) node;
                        printf("Found %d functions named %s\n", funcSet->functions.size(), identToken.string.c_str());
                        if (listIR) {
                            printf("Printing IR");
                            for (U32 i = 0; i < funcSet->functions.size(); i++) {
                                auto function = (ASTFunction*)funcSet->functions[i];
                                auto llvmFunction = (llvm::Function*)function->llvmFunction;
                                std::ostream& stdOstream = std::cout;
                                llvm::raw_os_ostream ostream(stdOstream);
                                ostream.changeColor(llvm::raw_os_ostream::Colors::GREEN);
                                llvmFunction->print(ostream);
                            }
                        } else {
                            for (U32 i = 0; i < funcSet->functions.size(); i++) {
                                auto function = (ASTFunction*)funcSet->functions[i];
                                Print((ASTFunction *) funcSet->functions[i]);
                            }
                        }

						printf("\n");
                    }
                        break;

                    case AST_STRUCT: {
                        auto structDefn = (ASTStruct *) node;
                        Print(structDefn);
                    }
                        break;

                    case AST_DEFINITION: {
                        printf("Really? You already know what that is.\n");
                    }
                        break;

                    default:
                        printf("Dont know what that is yet but it exists!");
                }
            }

        }

    }

	free(arena.memory);
}
