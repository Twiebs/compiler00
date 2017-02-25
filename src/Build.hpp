#pragma once

#include "AST.hpp"
#include "Parser.hpp"
#include "Lexer.hpp"

#include <thread>
#include <mutex>

#define FORCE_SINGLE_THREADED 1
#define ARENA_BLOCK_SIZE 4096
#define TEMP_BLOCK_SIZE 1 << 8

struct BuildSettings {
	std::string rootDir;
	std::string outputFile;

	bool logModuleDump;
	bool emitBitcode;
	bool emitAsm;
	bool emitIR;
	bool emitNativeOBJ;
	bool emitExecutable;
};

struct SourceFile {
  InternString path;
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
  //At the moment worker threads emit toplevel statements
  //into globalBlock using this mutex, since this is a realtivly
  //rare event contention should be minimal, but there are probably
  //much better solutions available
  std::mutex globalBlockMutex;
  ASTBlock globalBlock;

  U32 workerCount;
  Worker *workers;
  bool isWorkQueueActive;
  std::mutex workMutex;
  std::vector<SourceFile> addedFiles;
  std::vector<std::string> filesToParse; 
  std::vector<std::thread> activeThreads;

  //This mutex is used to increment the error count and
  //stop race conditions to calls to printf / other IO
  std::mutex errorMutex;
  U32 errorCount;

  //These are quick access pointers to the builtin types
  //that the language provides.  They are here to avoid having
  //to performe a string lookup based on their identifier
  ASTDefinition* global_voidType;
  ASTDefinition* global_U8Type;
  ASTDefinition* global_U16Type;
  ASTDefinition* global_U32Type;
  ASTDefinition* global_U64Type;
  ASTDefinition* global_S8Type;
  ASTDefinition* global_S16Type;
  ASTDefinition* global_S32Type;
  ASTDefinition* global_S64Type;
  ASTDefinition* global_F16Type;
  ASTDefinition* global_F32Type;
  ASTDefinition* global_F64Type;
  ASTDefinition* global_F128Type;

  BuildSettings buildSettings;
};

extern Compiler g_compiler;

void AddFileToParseQueue(Compiler *compiler, const std::string& filename);