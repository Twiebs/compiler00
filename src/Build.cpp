#include "Common.hpp"
#include "Build.hpp"

#include <string>
#include <iostream>
#include <fstream>

#include <chrono>
#include <atomic>
#include <thread>
#include <condition_variable>
#include <mutex>


void ParseFile (Worker* worker, const std::string& rootDir, const std::string& filename);
void CodegenPackage(Package* package, const BuildContext& context, BuildSettings* settings);
void ResolveAbstractSyntaxTree (Worker* worker);  // TODO consider ResolvePacakgeDeps or somthing

// ParsePackage()
// ResolvePackage()
// CodegenPackage()

int PreBuild(const BuildContext& context, const BuildSettings& settings) {
	return 0;
}

struct WorkQueue {
  std::mutex mutex;
  std::condition_variable cond;
	std::atomic<int> activeWorkers;
	std::atomic<int> workCount;
  std::vector<std::string> workList;
};

global_variable WorkQueue global_workQueue;
void PushWork (const std::string& filename) {
  global_workQueue.mutex.lock();
  global_workQueue.workList.push_back(filename);
	global_workQueue.workCount++;
  global_workQueue.mutex.unlock();
  global_workQueue.cond.notify_one();
  LOG_INFO("Added filename: " << filename << " to the global work queue");
}

internal void ThreadProc (Worker* worker, WorkQueue* workQueue, U32 threadID,  BuildSettings* settings) {
  bool working = true;

	auto getWork = [&]() -> const std::string {
		const std::string filename = workQueue->workList[workQueue->workList.size() - 1];
		workQueue->workList.pop_back();
		workQueue->workCount--;
		LOG_INFO("Thread " << threadID << " Popped filename: " << filename << " off the global work queue");
		return filename;
	};

	auto executeWork = [&](const std::string& filename) {
		workQueue->activeWorkers++;
	  ParseFile(worker, settings->rootDir, filename);
		workQueue->activeWorkers--;
		if (workQueue->activeWorkers == 0 && workQueue->workCount == 0) {
			workQueue->cond.notify_all();
			working = false;
			LOG_INFO("Thread " << threadID << " exiting");
		}
	};

  while (working) {
		workQueue->mutex.lock();
		if (workQueue->workList.size() > 0) {
			auto filename = getWork();
      workQueue->mutex.unlock();
			executeWork(filename);
		} else if (workQueue->activeWorkers == 0) {
			workQueue->cond.notify_all();
			workQueue->mutex.unlock();
			working = false;
			LOG_INFO("Thread " << threadID << " exiting because there was no work and no active workers");
		} else {
			workQueue->mutex.unlock();
			std::unique_lock<std::mutex> lock(workQueue->mutex);
	    workQueue->cond.wait(lock);

			// Our thread has been woken lets see if there is somthing to do
			if (workQueue->workList.size() > 0) {
				auto filename = getWork();
				lock.unlock();  // We have what we need... release the lock
				executeWork(filename);
			} else if (workQueue->activeWorkers == 0) {
				// in theory there is no reason the notify the other threads from this place
				// but im going to do it anyway because why not?
				workQueue->cond.notify_all();
				working = false;
				LOG_INFO("Thread " << threadID << " exiting");
			}
		}
	}
}

#define FORCE_SINGLE_THREADED 0
#define ARENA_BLOCK_SIZE 4096
#define TEMP_BLOCK_SIZE 1 << 8
int Build(BuildContext& context, BuildSettings& settings) {
  auto package = new Package;
  InitalizeLanguagePrimitives(&package->globalScope);
  context.packages.push_back(package);
  context.currentPackage = package;

  // Create workers for threads
#if FORCE_SINGLE_THREADED
	int workerCount = 1;
#else
	int workerCount = std::thread::hardware_concurrency();
#endif
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

  std::vector<std::thread> threads(workerCount - 1);
	if (workerCount > 1) {
	  for (auto i = workerCount - 2; i >= 0; i--) { // if 4 workers then start at index 2 end at 0
	      Worker* worker = &workers[i + 1]; // add a 1 because there are workerCount workers
	      threads[i] = std::thread(ThreadProc, worker, &global_workQueue, i + 1, &settings); // add one again because 0 is the main thread
	  }
	}

	// This is a quick hack to insure that there are workers in the global work queue before
	// we push some work onto the work stack
	using namespace std::literals;
	std::this_thread::sleep_for(1ms);
	PushWork(settings.inputFile);
	ThreadProc(&workers[0], &global_workQueue, 0, &settings);	// Kick of the main thread

	// The main thread has fallen out of its work loop, insure all other threads have also finished
	// execution and block untill they do so.
	if (workerCount > 1) {
		for (auto i = workerCount - 2; i >= 0; i--) {
			auto& thread = threads[i];
			thread.join();
		}
		// Now we resolve the dependices of our workers
		for(auto i = workerCount - 2; i >= 0; i--) {
			threads[i] = std::thread(ResolveAbstractSyntaxTree, &workers[i + 1]);
		}
	}

	ResolveAbstractSyntaxTree(&workers[0]);	// The main thread resolves its tree

	// The main thread has finsihed execution of AST resolution
	// we have no idea how much work is left for the other threads so
	// we block until they are all finished
	if (workerCount > 1) {
		for (auto i = workerCount - 2; i >= 0; i--) {
			auto& thread = threads[i];
			thread.join();
		}
	}
  // That's what i will do!
  // TODO we need a more robust way of doing this resolve dependencies work
  // Perhaps the best way to acomplish this is to do absoutly no working during the parsing phase of the AST
  // and then allways typecheck and resolve deps for every statement /expr in the tree after all files within a
  // package have been parsed

  // During the parsing phase the compiler will only check to make sure that you are not overwriting somthing that has allreayd been declared
	// ResolveDependencies(worker);
  // ResolveAbstractSyntaxTree(worker);

  U32 errorCount = 0;
  for (auto i = 0; i < workerCount; i++) {
    errorCount += workers[i].errorCount;
  }

  if (errorCount != 0) {
    LOG_ERROR("There were " << errorCount << " errors building the package");
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
