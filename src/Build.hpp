#pragma once

#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"

#include "AST.hpp"

/*
Build process

1. Construct the AST for each file included in the package
2. Resolve / Check / Infer Types
3. Codegen
*/

struct Package {
	ASTBlock globalScope;
	llvm::Module* module;
	U64 flags;
};

enum PackageFlags {
	PACKAGE_INVALID = 1 << 0,
};

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

//TODO setup language bultins here.
//or divert to acustom user tool with an api to do some preprocessor stuff or whatever
// build tool they want to use
//The Pre build and post build will return ints inorder to check for error codes
int PreBuild(const BuildContext& context, const BuildSettings& settings);

//When we build we allways parse so this is where the parsing will kick off
//After the parsing is complete we check someflags in the buildsettings to determine what to do with
//The created llvm::Module
int Build(BuildContext& context, BuildSettings& settings);

//TODO we can also do this exact same thing here but after the first build has been run
// I think that this is arelly good idea nad can provide a really great exensibility
int PostBuild(const BuildContext& context, const BuildSettings& settings);
