#include "Common.hpp"
#include "Build.hpp"
#include "Parser.hpp"

#include <string>
#include <iostream>
#include <fstream>
#include <thread>

void CodegenPackage(Package* package, const BuildContext& context, BuildSettings* settings);
void ResolveAbstractSyntaxTree (Worker* worker);  // TODO consider ResolvePacakgeDeps or somthing
// ParsePackage()
// ResolvePackage()
// CodegenPackage()

int PreBuild(const BuildContext& context, const BuildSettings& settings) {
	return 0;
}

#define ARENA_BLOCK_SIZE 4096
#define TEMP_BLOCK_SIZE 1 << 8
int Build(BuildContext& context, BuildSettings& settings) {
  auto package = new Package;
  InitalizeLanguagePrimitives(&package->globalScope);
  context.packages.push_back(package);
  context.currentPackage = package;

  // Create workers for threads
  int workerCount = std::thread::hardware_concurrency();
  U32 arenaMemorySize = ARENA_BLOCK_SIZE * workerCount;
  U32 tempMemorySize = workerCount * TEMP_BLOCK_SIZE;
  U32 memorySize = (sizeof(Worker) * workerCount) + arenaMemorySize + tempMemorySize;
  void* workerMemory = malloc(memorySize);
  LOG_INFO("Created " << workerCount << " workers! and allocated inital memory sized: " << memorySize);

  Worker* workers = (Worker*)workerMemory;
  U8* arenaMemory = (U8*)(workers + workerCount);
  for (auto i = 0; i < workerCount; i++) {
    Worker* worker = &workers[i];
    worker = new (worker) Worker;
    worker->currentScope = &package->globalScope;
    worker->arena.memory = arenaMemory + (i * ARENA_BLOCK_SIZE);
    worker->arena.capacity = ARENA_BLOCK_SIZE;
  }

  // TODO Push some tasks into a global queue and then kick off the parsing
  // Process with the worker threads

  Worker* worker = &workers[0];
	ParseFile(worker, settings.rootDir, settings.inputFile);

	while (worker->workQueue.size() > 0) {
		auto filename = worker->workQueue[worker->workQueue.size() - 1];
		worker->workQueue.pop_back();
		ParseFile(worker, settings.rootDir, filename);
	}

  // That's what i will do!
  // TODO we need a more robust way of doing this resolve dependencies work
  // Perhaps the best way to acomplish this is to do absoutly no working during the parsing phase of the AST
  // and then allways typecheck and resolve deps for every statement /expr in the tree after all files within a
  // package have been parsed

  // During the parsing phase the compiler will only check to make sure that you are not overwriting somthing that has allreayd been declared
	//ResolveDependencies(worker);
  ResolveAbstractSyntaxTree(worker);

  if (worker->errorCount != 0) {
    LOG_ERROR("There were " << worker->errorCount << " errors building the package");
    return -1;
  }

  CodegenPackage(package, context, &settings);
  free(workerMemory);
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
