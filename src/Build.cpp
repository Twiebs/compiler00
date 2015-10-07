#include "Common.hpp"
#include "Build.hpp"

#include <chrono>
#include <atomic>
#include <thread>
#include <condition_variable>
#include <mutex>

struct WorkQueue {
    std::mutex mutex;
    std::condition_variable cond;
    std::atomic<int> activeWorkers;
    std::atomic<int> workCount;
    std::vector<std::string> importedFiles;
    std::vector<std::string> workList;	// TODO remove this std::vector<std::string> insanity
};

struct Workspace {
    U32 workerCount;
    Worker* workers;
    WorkQueue workQueue;
    void* memory;

    std::vector<Package*> packages;
};

void RunInterp (Package* package);
void ParseFile (Worker* worker, const std::string& rootDir, const std::string& filename);
void CodegenPackage(Package* package, BuildSettings* settings);
void AnalyzeAST (Worker* worker);



global_variable Workspace global_workspace;


// HACK
global_variable BuildSettings global_settings;

void PushWork (const std::string& filename) {
    WorkQueue* queue = &global_workspace.workQueue;
    queue->mutex.lock();
    bool containsFilename = false;
    for (auto& str : queue->importedFiles) {
        if (!str.compare(filename)) {
            containsFilename = true;
        }
    }

    if (!containsFilename) {
        queue->importedFiles.push_back(filename);
        queue->workList.push_back(filename);
        queue->workCount++;
        queue->cond.notify_one();
        LOG_DEBUG("Added filename: " << filename << " to the global work queue");
    }
    queue->mutex.unlock();
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

// TODO InitWorkspace should be able to be done lazily if the interp is run instead of directly
// calling the compiler to run on a file / package
// XXX Threading appears to be broken when analasis happens
// They are all acting upon the same memory
#define FORCE_SINGLE_THREADED 1
#define ARENA_BLOCK_SIZE 4096
#define TEMP_BLOCK_SIZE 1 << 8
internal void InitWorkspace (Workspace* workspace) {
    workspace->workerCount = FORCE_SINGLE_THREADED ?  1 : std::thread::hardware_concurrency();
    U32 arenaMemorySize = ARENA_BLOCK_SIZE * workspace->workerCount ;
    U32 tempMemorySize = workspace->workerCount * TEMP_BLOCK_SIZE;
    size_t memorySize = (sizeof(Worker) * workspace->workerCount ) + arenaMemorySize + tempMemorySize;
    workspace->memory = malloc(memorySize);
    workspace->workers = (Worker*)workspace->memory;

    Worker* workers = (Worker*)workspace->memory;
    U8* arenaMemory = (U8*)(workers + workspace->workerCount );
    U8* tempMemory = arenaMemory + arenaMemorySize;
    for (U32 i = 0; i < workspace->workerCount ; i++) {
        Worker* worker = &workers[i];
        worker = new (worker) Worker;
        worker->tempMemory = tempMemory + (i * TEMP_BLOCK_SIZE);
        worker->arena.memory = arenaMemory + (i * ARENA_BLOCK_SIZE);
        worker->arena.capacity = ARENA_BLOCK_SIZE;
    }
}

internal void FreeSubArenas (MemoryArena* arena) {
    if (arena->next != nullptr) {
        FreeSubArenas(arena->next);
        free(arena->next);
    }
}

internal void ExitWorkspace (Workspace* workspace) {
    for (U32 i = 0; i < workspace->workerCount; i++)
        FreeSubArenas(&workspace->workers[i].arena);
    free(workspace->memory);
}

void Build () {
    // HACK to keep working with current build system
    Package thePackage;

    Package* package = &thePackage;
    package->arena.memory = malloc(ARENA_BLOCK_SIZE);
    package->arena.capacity = ARENA_BLOCK_SIZE;
    package->arena.used = 0;

    auto& settings = global_settings;
    Workspace* workspace = &global_workspace;
    Worker* mainWorker = &global_workspace.workers[0];

    InitalizeLanguagePrimitives(&mainWorker->arena, &package->globalBlock);
    std::string typeName = "U8";
    auto typeDefn = (ASTDefinition*)FindNodeWithIdent(&package->globalBlock, typeName);

    for (U32 i = 0; i < workspace->workerCount; i++) {
        Worker* worker = &workspace->workers[i];
        worker->currentBlock = &package->globalBlock;
        worker->currentPackage = package;
    }

	std::vector<std::thread> threads(workspace->workerCount - 1);
	if (workspace->workerCount > 1) {
		for (int i = workspace->workerCount - 2; i >= 0; i--) { // if 4 workers then start at index 2 end at 0
				Worker* worker = &workspace->workers[i + 1]; // add a 1 because there are workerCount workers
				threads[i] = std::thread(ThreadProc, worker, &workspace->workQueue, i + 1, &settings); // add one again because 0 is the main thread
		}
	}

	// This is a quick hack to insure that there are workers in the global work queue before
	// we push some work onto the work stack
	using namespace std::literals;
	std::this_thread::sleep_for(1ms);
	PushWork(settings.inputFile);
	ThreadProc(&workspace->workers[0], &workspace->workQueue, 0, &settings);	// Kick of the main thread

	// The main thread has fallen out of its work loop, insure all other threads have also finished
	// execution and block untill they do so.
	if (workspace->workerCount > 1) {
		for (int i = workspace->workerCount - 2; i >= 0; i--) {
			auto& thread = threads[i];
			thread.join();
		}
	}

	LOG_INFO("Parsing complete");


	// Now we resolve the dependices of our workers
	if (workspace->workerCount > 1) {
		for(S32 i = workspace->workerCount - 2; i >= 0; i--) {
			threads[i] = std::thread(AnalyzeAST, &workspace->workers[i + 1]);
		}
	}


	LOG_INFO("Analyzing Package");
	AnalyzeAST(&workspace->workers[0]);	// The main thread resolves its tree

	// The main thread has finsihed execution of AST resolution
	// we have no idea how much work is left for the other threads so
	// we block until they are all finished
	if (workspace->workerCount > 1) {
		for (S32 i = workspace->workerCount - 2; i >= 0; i--) {
			auto& thread = threads[i];
			thread.join();
		}
	}
	LOG_INFO("Analysis Complete");

	U32 errorCount = 0;
	for (S32 i = 0; i < workspace->workerCount; i++) {
		errorCount += workspace->workers[i].errorCount;
	}

	if (errorCount != 0) {
		LOG_ERROR("There were " << errorCount << " errors building the package");
		return;
	}

	LOG_INFO("Generating Package");
	CodegenPackage(package, &global_settings);

    // Test code to see how the interp works
    RunInterp(package);

    FreeSubArenas(&package->arena);
}

void RunInterpTest();

int main (int argc, char** argv) {
    InitWorkspace(&global_workspace);

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
		LOG_INFO("Running Interpreter... Not now...");
		// RunInterp();
        ExitWorkspace(&global_workspace);
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

    ExitWorkspace(&global_workspace);
	return 0;
}
