#pragma once
#include <fstream>
#include "AST.hpp"

struct Package {
    std::string name;
    ASTBlock globalBlock;
};

struct Worker {
	FILE* file = nullptr;
	char lastChar = 0, nextChar = 0;
	U32 lineNumber = 0, colNumber = 0;
	int currentIndentLevel = 0;

	Token token;

    U32 errorCount = 0;
    MemoryArena arena;
    U8* tempMemory;
    U8* currentTempLocation;

    ASTBlock* currentBlock = nullptr;
};

enum PackageFlags {
	PACKAGE_INVALID = 1 << 0,
};

struct BuildSettings {
	std::string rootDir;
	std::string inputFile; // For now this is just a singular thing for simplicity
	std::string packageName;
	std::string outputFile;

	std::vector<std::string> libDirs;
	std::vector<std::string> libNames;
	std::vector<std::string> importDirs;

	bool logModuleDump;
	bool emitBitcode;
	bool emitAsm;
	bool emitIR;
	bool emitNativeOBJ;
	bool emitExecutable;
};

extern "C" void Build ();
