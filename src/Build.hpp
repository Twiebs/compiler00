#pragma once

#include "AST.hpp"
#include "Parser.hpp"
#include "Lexer.hpp"

#include <thread>
#include <mutex>

#define FORCE_SINGLE_THREADED 1
#define ARENA_BLOCK_SIZE 4096
#define TEMP_BLOCK_SIZE 1 << 8

std::ostream& ReportError (const FileSite& site);
std::ostream& ReportError();
std::ostream& ReportError(const SourceLocation& sourceLocation);
#define ReportSourceError(sourceLocation, msg) ReportError(sourceLocation) << msg << "\n"

struct BuildSettings {
	std::string rootDir;
	std::string inputFile;
	std::string packageName;
	std::string outputFile;

	bool logModuleDump;
	bool emitBitcode;
	bool emitAsm;
	bool emitIR;
	bool emitNativeOBJ;
	bool emitExecutable;
};

struct Worker {
	U32 workerID;
  U32 errorCount;
  ASTBlock *currentBlock;

  MemoryArena arena;
  U8* tempMemory;
  Lexer lex;
};

struct Compiler {
  ASTBlock globalBlock;
  U32 workerCount;
  Worker *workers;
  U32 errorCount;

  bool isWorkQueueActive;
  std::mutex workQueueMutex;
  std::vector<std::string> fileList;
  std::vector<std::string> filesToParse; 
  std::vector<std::thread> activeThreads;

  BuildSettings buildSettings;
};

void AddFileToParseQueue(Compiler *compiler, const std::string& filename);