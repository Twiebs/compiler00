#pragma once
#include <fstream>
#include "AST.hpp"

// Serious question why the fuck do we care about the llvmModule
// here??? we have not even created it yet!!!!
struct Package {
	ASTBlock globalScope;
	U64 flags;
};

struct Worker {
	FILE* file = nullptr;
	char lastChar = 0, nextChar = 0;
	U32 lineNumber = 0, colNumber = 0;
	int currentIndentLevel = 0;
	Token token;

  ASTBlock* currentScope = nullptr;
  U32 errorCount = 0;
  MemoryArena arena;
  std::vector<std::string> workQueue;
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

// NOTE consider seperating errorCount into a buildState
// We could then have a global_buildState;
// its a hack but we dont need to tote that information around with the context!
struct BuildContext {
	std::vector<Package*> packages;
	Package* currentPackage;
	U32 errorCount;
};

extern "C" int PreBuild(const BuildContext& context, const BuildSettings& settings);
extern "C" int Build(BuildContext& context, BuildSettings& settings);
extern "C" int PostBuild(const BuildContext& context, const BuildSettings& settings);
