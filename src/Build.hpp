#pragma once
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"

#include "AST.hpp"

struct Package {
	ASTBlock globalScope;
	llvm::Module* module;
	U64 flags;
};

enum PackageFlags {
	PACKAGE_INVALID = 1 << 0,
};

struct CallDependency {
  std::string identName;
  ASTCall* call;
};

void AddDependency(const std::string& identName, ASTCall* call);

struct BuildSettings {
	std::string rootDir;
	std::string inputFile; // For now this is just a singular thing for simplicity
	std::string outputFile;

	std::vector<std::string> libDirs;
	std::vector<std::string> libNames;
	std::vector<std::string> importDirs;

	bool logModuleDump;
	bool emitBitcode;
	bool emitAsm;
	bool emitNativeOBJ;
	bool emitExecutable;
};

//NOTE consider seperating errorCount into a buildState
//We could then have a global_buildState;
//its a hack but we dont need to tote that information around with the context!
struct BuildContext {
	std::vector<Package*> packages;
	Package* currentPackage;
	llvm::IRBuilder<>* builder;
	U32 errorCount;
};

extern "C" int PreBuild(const BuildContext& context, const BuildSettings& settings);
extern "C" int Build(BuildContext& context, BuildSettings& settings);
extern "C" int PostBuild(const BuildContext& context, const BuildSettings& settings);
