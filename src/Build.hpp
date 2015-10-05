#pragma once
#include <fstream>
#include "AST.hpp"
#include <mutex>

// We need a ASTPrototype because if we store functions in a package
// and then we let its consituant members fall out of scope then there is no
// reason to have the member defintions at all...
// The block is then completly irrelevant
// Then function prototypes could be stored directly inside of the package
// along with structs, global variables and whatever else is required
// mabye macro definitions?? or somthing similar?
// This is an interesting idea because perhaps the current way the function resolver works then
// could be changed to directly work on prototypes rather than concrete functions
// Yeah thats exactly what you want to happen!
// Internal functions could be just stored within the regular AST but that might become to cumbersome?
// No thats fine it would work well enough.

struct Package {
    std::string name;
    ASTBlock globalBlock;
    MemoryArena arena;
    std::mutex mutex;
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

    Package* currentPackage = nullptr;
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
