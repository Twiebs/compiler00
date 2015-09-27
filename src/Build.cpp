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

#include "Analysis.cpp"

void RunInterp();
void ParseFile (Worker* worker, const std::string& rootDir, const std::string& filename);
void CodegenPackage(Package* package, BuildSettings* settings);
void AnalyzeAST (Worker* worker);	// TODO consider ResolvePacakgeDeps or somthing

int PreBuild(const BuildContext& context, const BuildSettings& settings) {
	return 0;
}

global_variable BuildSettings global_settings;

struct WorkQueue {
	std::mutex mutex;
	std::condition_variable cond;
	std::atomic<int> activeWorkers;
	std::atomic<int> workCount;
	std::vector<std::string> workList;	// TODO remove this std::vector<std::string> insanity
};

global_variable WorkQueue global_workQueue;
void PushWork (const std::string& filename) {
	global_workQueue.mutex.lock();
	global_workQueue.workList.push_back(filename);
	global_workQueue.workCount++;
	global_workQueue.mutex.unlock();
	global_workQueue.cond.notify_one();
	LOG_DEBUG("Added filename: " << filename << " to the global work queue");
}

internal void ThreadProc (Worker* worker, WorkQueue* workQueue, U32 threadID, BuildSettings* settings) {
	bool working = true;

	auto getWork = [&]() -> const std::string {
		const std::string filename = workQueue->workList[workQueue->workList.size() - 1];
		workQueue->workList.pop_back();
		workQueue->workCount--;
		LOG_DEBUG("Thread " << threadID << " Popped filename: " << filename << " off the global work queue");
		return filename;
	};

	auto executeWork = [&](const std::string& filename) {
		workQueue->activeWorkers++;
		ParseFile(worker, settings->rootDir, filename);
		workQueue->activeWorkers--;
		if (workQueue->activeWorkers == 0 && workQueue->workCount == 0) {
			workQueue->cond.notify_all();
			working = false;
			LOG_DEBUG("Thread " << threadID << " exiting");
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
			LOG_DEBUG("Thread " << threadID << " exiting");
		} else {
			workQueue->mutex.unlock();
			std::unique_lock<std::mutex> lock(workQueue->mutex);
			workQueue->cond.wait(lock);

			// Our thread has been woken lets see if there is somthing to do
			if (workQueue->workList.size() > 0) {
				auto filename = getWork();
				lock.unlock();	// We have what we need... release the lock
				executeWork(filename);
			} else if (workQueue->activeWorkers == 0) {
				// in theory there is no reason the notify the other threads from this place
				// but im going to do it anyway because why not?
				workQueue->cond.notify_all();
				working = false;
				LOG_DEBUG("Thread " << threadID << " exiting");
			}
		}
	}
}

// BuildContext is very irrelevant
// Build should take a package which is given an inital file / name / whatever
// HACK Quick hacks to test the interpereter and how it will function
global_variable std::vector<std::string> global_filenames;
extern "C" void AddPackage(const char* filename) {
    global_filenames.push_back(std::string(filename));
}

#define FORCE_SINGLE_THREADED 1
internal inline U32 GetWorkerCount() {
#if FORCE_SINGLE_THREADED
    U32 workerCount = 1;
#else
    U32 workerCount = std::thread::hardware_concurrency();
#endif
    return workerCount;
}

#define ARENA_BLOCK_SIZE 4096
#define TEMP_BLOCK_SIZE 1 << 8
void Build () {
    Package thePackage;
    auto package = &thePackage;

    auto workerCount = GetWorkerCount();
	U32 arenaMemorySize = ARENA_BLOCK_SIZE * workerCount;
	U32 tempMemorySize = workerCount * TEMP_BLOCK_SIZE;
	size_t memorySize = (sizeof(Worker) * workerCount) + arenaMemorySize + tempMemorySize;
	void* workerMemory = malloc(memorySize);
	LOG_INFO("Created " << workerCount << " workers! Allocated inital memory: " << memorySize << " bytes");

	Worker* workers = (Worker*)workerMemory;
	U8* arenaMemory = (U8*)(workers + workerCount);
	for (auto i = 0; i < workerCount; i++) {
		Worker* worker = &workers[i];
		worker = new (worker) Worker;
		worker->currentScope = &package->globalScope;
		worker->arena.memory = arenaMemory + (i * ARENA_BLOCK_SIZE);
		worker->arena.capacity = ARENA_BLOCK_SIZE;
	}

    // HACK to keep working with current build system
    auto& settings = global_settings;


    InitalizeLanguagePrimitives(&package->globalScope);


	// TODO consider pushing these threads on to the transient state of the main thread or something
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
	}

	LOG_INFO("Parsing complete");


	// Now we resolve the dependices of our workers
	if (workerCount > 1) {
		for(auto i = workerCount - 2; i >= 0; i--) {
			threads[i] = std::thread(AnalyzeAST, &workers[i + 1]);
		}
	}


	LOG_INFO("Analyzing Package");
	AnalyzeAST(&workers[0]);	// The main thread resolves its tree

	// The main thread has finsihed execution of AST resolution
	// we have no idea how much work is left for the other threads so
	// we block until they are all finished
	if (workerCount > 1) {
		for (auto i = workerCount - 2; i >= 0; i--) {
			auto& thread = threads[i];
			thread.join();
		}
	}
	LOG_INFO("Analysis Complete");
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
		return;
	}

	LOG_INFO("Generating Package");
	CodegenPackage(package, &global_settings);
	free(workerMemory);
}

int PostBuild(const BuildContext& context, const BuildSettings& settings) {
	return 0;
}

int main (int argc, char** argv) {
	auto& settings = global_settings;
	settings.libDirs.push_back("../build/libcpp");
	settings.libNames.push_back("std");
	settings.libNames.push_back("SDL");
	settings.libNames.push_back("GL");

	settings.emitIR = true;
	settings.emitNativeOBJ = true;
	settings.emitExecutable = true;

	// Get the input filename
	if (argc < 2) {
        settings.inputFile = "test.src";
		LOG_INFO("Running Interpreter...");
		RunInterp();
		return 0;
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

	auto lastDot = input.find_last_of(".");
	auto packageName = input.substr(0, lastDot);
	settings.packageName = packageName;
	settings.inputFile = input;

	Build();

	return 0;
}
