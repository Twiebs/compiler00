#include <string>
#include <iostream>
#include <fstream>
#include <system_error>
#include <unistd.h>

#include "llvm/Support/CommandLine.h"
#include "Build.hpp"

int main (int argc, char** argv) {
	BuildSettings settings;
	settings.libDirs.push_back("build/libcpp");
	settings.libNames.push_back("std");
	settings.libNames.push_back("SDL");
	settings.libNames.push_back("GL");

	settings.logModuleDump = true;
	settings.emitNativeOBJ = true;
	settings.emitExecutable = true;

	BuildContext context;
	context.builder = new llvm::IRBuilder<>(llvm::getGlobalContext());

	static llvm::cl::opt<std::string> inputFile(llvm::cl::Positional, llvm::cl::desc("<input file>"));
	static llvm::cl::opt<std::string> outputFile("o", llvm::cl::desc("Output filename"), llvm::cl::value_desc("filename"));
	llvm::cl::ParseCommandLineOptions(argc, argv);

	if (inputFile == "") {
		LOG_ERROR("You must specify the filename of the program to be compiled.  Use --help to see the options.");
		abort();
	}

	std::string input = inputFile;
	auto lastSlash = input.find_last_of("/");
	if(lastSlash == std::string::npos) {
		settings.rootDir = "";
	} else {
		settings.rootDir = input.substr(0, lastSlash + 1);
		input.erase(0, lastSlash + 1);
	}

	settings.inputFile = input;

	PreBuild(context, settings);
	Build(context, settings);
	PostBuild(context, settings);

	return 0;
}
