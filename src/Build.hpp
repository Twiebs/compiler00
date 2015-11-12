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

#include "llvm/IR/Module.h"

#include "Parser.hpp"
#include "Lexer.hpp"

std::ostream& ReportError (const FileSite& site);
std::ostream& ReportError ();

struct Package {
    std::string name;
    ASTBlock rootBlock;
    MemoryArena arena;
    std::mutex mutex;
    llvm::Module* llvmModule;
};

struct Worker {
    U32 errorCount = 0;
    MemoryArena arena;
    U8* tempMemory;

    Lexer lex;
    ASTBlock* currentBlock = nullptr;
    Package* currentPackage = nullptr;
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
