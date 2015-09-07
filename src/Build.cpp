#include "Build.hpp"
#include "Parser.hpp"

#include <string>
#include <iostream>
#include <fstream>
#include <system_error>
#include <unistd.h>

#include "llvm/Support/CommandLine.h"

void CodegenPackage(Package* package, const BuildContext& context);
void WriteBitcode(llvm::Module* module, const std::string& outputFile);
int WriteNativeObject(llvm::Module* module, const BuildSettings& settings);
int WriteExecutable(BuildSettings& settings);

global_variable std::vector<CallDependency> global_calldeps;
void AddDependency(const std::string& identName, ASTCall* call) {
  global_calldeps.push_back({identName, call});
}

void ResolveDependencies(ParseState& state) {
  for(auto i = 0; i < global_calldeps.size(); i++) {
    auto& dep = global_calldeps[i];
    auto ident = FindIdentifier(state.currentScope, dep.identName);
    if (ident == nullptr) {
    	FileSite site;
      ReportError(state, site, "Could not find any function matching the identifier: " + dep.identName);
      break;
    }
    auto funcSet = (ASTFunctionSet*)ident->node;
    assert(funcSet->nodeType == AST_FUNCTION);
    auto args = (ASTExpression**)(dep.call + 1);
    dep.call->function = FindFunction(funcSet, args, dep.call->argCount);
    if (!dep.call->function) {
      FileSite site;
      ReportError(state, site, "Could not match argument types to any function named: " + dep.identName);
    }
  }
}

int PreBuild(const BuildContext& context, const BuildSettings& settings) {
	return 0;
}

int Build(BuildContext& context, BuildSettings& settings) {
	// Package Must Persist pass this stage
	auto package = new Package;
	package->module = new llvm::Module("LLVMLang Compiler (TODO PackageName)", llvm::getGlobalContext());
	InitalizeLanguagePrimitives(&package->globalScope, package->module);
	context.packages.push_back(package);
	context.currentPackage = package;

	ParseState parseState;
	parseState.currentScope = &package->globalScope;
	parseState.settings = &settings;
	parseState.arena.memory = malloc(4096);
	parseState.arena.capacity = 4096;
	ParseFile(parseState, settings.rootDir, settings.inputFile);

	while (parseState.importedFiles.size() > 0) {
		auto filename = parseState.importedFiles[parseState.importedFiles.size() - 1];
		parseState.importedFiles.pop_back();
		ParseFile(parseState, settings.rootDir, filename);
	}
	ResolveDependencies(parseState);

	if (parseState.errorCount == 0) {
    LOG_INFO("Emitting code for package...")
		CodegenPackage(package, context);
		if (settings.logModuleDump) {
			package->module->dump();
		}

		if (settings.outputFile == "") {
			auto inputBase = settings.inputFile.substr(0, settings.inputFile.find(".") + 1);
			settings.outputFile = settings.rootDir + inputBase + "o";
		}

		if (settings.emitNativeOBJ)
			WriteNativeObject(package->module, settings);
		if (settings.emitExecutable)
			WriteExecutable(settings);
	} else {
		LOG_ERROR("There were errors building the package");
		return -1;
	}
	return 0;
}

int PostBuild(const BuildContext& context, const BuildSettings& settings) {
	return 0;
}


int WriteExecutable(BuildSettings& settings) {
	std::string allLibs;
	for(auto& dir : settings.libDirs)
		allLibs.append("-L" + settings.rootDir + dir + " ");
	for(auto& lib : settings.libNames)
		allLibs.append("-l" + lib + " ");

	std::string cmd = "clang++ " + settings.outputFile + " " + allLibs + " -o " + settings.rootDir + "app";
	LOG_INFO("Writing Executable: " << cmd);
	system(cmd.c_str());
	return 0;
}

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
