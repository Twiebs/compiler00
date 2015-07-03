//NOTE all of this stuff was created on June18 if you end of caring about that sort of thing!
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

uint32 gErrorCount = 0;

int main(int argc, char** argv) {
	gErrorCount = 0;
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

	if(gErrorCount > 0) {
		LOG_ERROR("There were " << gErrorCount - 1<< " errors!");
	} else {
		LOG_INFO("No Errors were reported!  Have a \x1b[31mW\x1b[32mo\x1b[33mn\x1b[34md\x1b[35me\x1b[36mr\x1b[31mf\x1b[32mu\x1b[33ml\x1b[34ml \x1b[39mday!");
	}

	std::error_code errorCode;
	llvm::raw_fd_ostream ostream(outputFile, errorCode, llvm::sys::fs::F_None);
	llvm::WriteBitcodeToFile(module, ostream);
	LOG_INFO("Writing bitcode to file");
	return 0;
}
