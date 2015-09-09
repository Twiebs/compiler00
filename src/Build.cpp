#include "Common.hpp"
#include "Build.hpp"
#include "Parser.hpp"

#include <string>
#include <iostream>

void CodegenPackage(Package* package, const BuildContext& context, BuildSettings* settings);

global_variable std::vector<CallDependency> global_calldeps;
void AddDependency(const std::string& identName, ASTCall* call) {
  global_calldeps.push_back({identName, call});
}

void ResolveDependencies(Worker* worker) {
  for(auto i = 0; i < global_calldeps.size(); i++) {
    auto& dep = global_calldeps[i];
    auto ident = FindIdentifier(worker->currentScope, dep.identName);
    if (ident == nullptr) {
    	FileSite site;
      ReportError(worker, site, "Could not find any function matching the identifier: " + dep.identName);
      break;
    }
    auto funcSet = (ASTFunctionSet*)ident->node;
    assert(funcSet->nodeType == AST_FUNCTION);
    auto args = (ASTExpression**)(dep.call + 1);
    dep.call->function = FindFunction(funcSet, args, dep.call->argCount);
    if (!dep.call->function) {
      FileSite site;
      ReportError(worker, site, "Could not match argument types to any function named: " + dep.identName);
    }
  }
}

int PreBuild(const BuildContext& context, const BuildSettings& settings) {
	return 0;
}

int Build(BuildContext& context, BuildSettings& settings) {
	auto package = new Package;
	InitalizeLanguagePrimitives(&package->globalScope);
	context.packages.push_back(package);
	context.currentPackage = package;

	Worker worker;
	worker.currentScope = &package->globalScope;
	worker.arena.memory = malloc(4096);
	worker.arena.capacity = 4096;
	worker.errorCount = 0;
	ParseFile(&worker, settings.rootDir, settings.inputFile);

	while (worker.workQueue.size() > 0) {
		auto filename = worker.workQueue[worker.workQueue.size() - 1];
		worker.workQueue.pop_back();
		ParseFile(&worker, settings.rootDir, filename);
	}
	ResolveDependencies(&worker);

	if (worker.errorCount == 0) {
    LOG_INFO("Emitting code for package...")
		CodegenPackage(package, context, &settings);
	} else {
		LOG_ERROR("There were errors building the package");
		return -1;
	}
	return 0;
}

int PostBuild(const BuildContext& context, const BuildSettings& settings) {
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

  // Get the input filename
  if (argc < 2) {
    LOG_ERROR("You must specify the filename of the program to be compiled.  Use --help to see the options.");
    return -1;
  }

  const char* inputFilename = argv[1];
  auto input = std::string(inputFilename);
	auto lastSlash = input.find_last_of("/");
	if (lastSlash == std::string::npos) {
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
