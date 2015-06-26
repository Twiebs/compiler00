/*
 * Main.cpp
 *
 *  Created on: Jun 14, 2015
 *      Author: Torin Wiebelt
 */

#include <string>
#include <iostream>
#include <fstream>
#include <system_error>

#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/FormattedStream.h"

#include "llvm/Bitcode/ReaderWriter.h"

#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/GenericValue.h"
#include "llvm/ExecutionEngine/GenericValue.h"
#include "llvm/ExecutionEngine/MCJIT.h"

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Attributes.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Type.h"

#include "Lexer.hpp"
#include "Parser.hpp"

extern "C"
void println() {
	std::cout << "This is a message send from my language!\n";
}


int main(int argc, char** argv) {
	static llvm::cl::opt<std::string> inputFile(llvm::cl::Positional,
			llvm::cl::desc("<input file>"));
	static llvm::cl::opt<std::string> outputFile("o",
			llvm::cl::desc("Output filename"),
			llvm::cl::value_desc("filename"));
	llvm::cl::ParseCommandLineOptions(argc, argv);

	if (inputFile == "") {
		std::cout << "Error: You must specify the filename of the program to "
		"be compiled.  Use --help to see the options.\n";
		abort();
	}

	if(outputFile == "") {
		std::string base = inputFile;
		base = base.substr(0, base.size() - 3);
		outputFile = base + "bc";
	}

	std::ifstream stream(inputFile.c_str());
	if(!stream.is_open()) {
		std::cout << "Error: Can not open file: " << inputFile << "\n";
		abort();
	}

	llvm::LLVMContext& context = llvm::getGlobalContext();
	llvm::Module* module = new llvm::Module("LLVMLang Compiler", context);

	Parser parser(module, inputFile, &stream);
	parser.ParseFile();

	std::error_code errorCode;
	llvm::raw_fd_ostream ostream(outputFile, errorCode, llvm::sys::fs::F_None);
	llvm::WriteBitcodeToFile(module, ostream);
	LOG_INFO("Writing bitcode to file");

	//Move theModule into the execution engine
	// llvm::ExecutionEngine* engine = llvm::EngineBuilder(std::unique_ptr<llvm::Module>(module)).create();
	//
	// auto mainFunction = engine->FindFunctionNamed("Main");
	// if (mainFunction == nullptr) {
	// 	LOG_ERROR("Program must contain a 'Main' function!");
	// 	return 0;
	// }
	// std::vector<llvm::GenericValue> args;
	// llvm::GenericValue returnCode = engine->runFunction(mainFunction, args);

	//system("pause");
	return 0;
}
