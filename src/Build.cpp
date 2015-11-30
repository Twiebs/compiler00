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
	U32 errorCount;

    Worker* workers;
    WorkQueue workQueue;
    void* memory;

    std::vector<Package*> packages;
	std::vector<std::thread> threads;
	std::mutex errorMutex;
};

void RunInterp (Package* package);
int ParseFile(Worker* worker, const std::string& rootDir, const std::string& filename);
void CodegenPackage(Package* package, BuildSettings* settings);
void AnalyzeAST (Worker* worker);

std::ostream& operator<<(std::ostream& stream, const SourceLocation& sourceLocation) {
	stream << "[" << sourceLocation.filename << " " << sourceLocation.lineNumber << ":" << sourceLocation.columnNumber << "]";
	return stream;
}

global_variable Workspace global_workspace;

std::ostream& ReportError() {
	global_workspace.errorCount++;
	return std::cerr;
}

std::ostream& ReportError(const SourceLocation& sourceLocation) {
	global_workspace.errorCount++;
	std::cerr << sourceLocation;
	return std::cerr;
}



// HACKS
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

static void ThreadProc (Worker* worker, WorkQueue* workQueue, U32 threadID, BuildSettings* settings) {
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

static void FreeSubArenas(MemoryArena* arena) {
	if (arena->next != nullptr) {
		FreeSubArenas(arena->next);
		free(arena->next);
	}
}


#define FORCE_SINGLE_THREADED 1
#define ARENA_BLOCK_SIZE 4096
#define TEMP_BLOCK_SIZE 1 << 8
static void InitWorkspace (Workspace* workspace) {
    workspace->workerCount = FORCE_SINGLE_THREADED ?  1 : std::thread::hardware_concurrency();
    U32 arenaMemorySize = ARENA_BLOCK_SIZE * workspace->workerCount ;
    U32 tempMemorySize = workspace->workerCount * TEMP_BLOCK_SIZE;
    size_t memorySize = (sizeof(Worker) * workspace->workerCount) + arenaMemorySize + tempMemorySize;
    workspace->memory = malloc(memorySize);
    workspace->workers = (Worker*)workspace->memory;
	global_workspace.errorCount = 0;

    Worker* workers = (Worker*)workspace->memory;
    U8* arenaMemory = (U8*)(workers + workspace->workerCount );
    U8* tempMemory = arenaMemory + arenaMemorySize;
    for (U32 i = 0; i < workspace->workerCount ; i++) {
        Worker* worker = &workers[i];
        worker = new (worker) Worker;
		worker->workerID = i;
        worker->tempMemory = tempMemory + (i * TEMP_BLOCK_SIZE);
        worker->arena.memory = arenaMemory + (i * ARENA_BLOCK_SIZE);
        worker->arena.capacity = ARENA_BLOCK_SIZE;
    }
}

static void ExitWorkspace(Workspace* workspace) {
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

    InitalizeLanguagePrimitives(&mainWorker->arena, &package->rootBlock);
    std::string typeName = "U8";
    auto typeDefn = (ASTDefinition*)FindNodeWithIdent(&package->rootBlock, typeName);

    for (U32 i = 0; i < workspace->workerCount; i++) {
        Worker* worker = &workspace->workers[i];
        worker->currentBlock = &package->rootBlock;
        worker->currentPackage = package;
    }


	// Create the threads for our workspace and begin the thread procedure with each worker
	workspace->threads.resize(workspace->workerCount - 1);
	for (auto i = 1; i < static_cast<int>(workspace->workerCount); i++) {
		auto worker = &workspace->workers[i];
		workspace->threads[i - 1] = std::thread(ThreadProc, worker, &workspace->workQueue, i, &settings);
	}

	// This is a quick hack to insure that there are workers in the global work queue before
	// we push some work onto the work stack
	using namespace std::literals;
	std::this_thread::sleep_for(1ms);
	PushWork(settings.inputFile);
	// Kick of the main thread
	ThreadProc(&workspace->workers[0], &workspace->workQueue, 0, &settings);	


	// TODO if the main thread falls out since there is no work avaible and another worker adds a file to import
	// that imported file could import an enormous amount of files and the main thread would sit around doing nothing

	// This procedure will be called when the main thread has finished its current task
	// and there is no other work avaible for it to compleate.  The main thread waits for
	// the other workers in seprate threads to compleate the work they are executing.
	static auto JoinAllWorkersInOtherThreads = [](Workspace* workspace) {
		for (auto i = 1; i < static_cast<int>(workspace->workerCount); i++) {
			auto& thread = workspace->threads[i - 1];
			thread.join();
		}
	};

	static auto GetCurrentErrorCountFromAllWorkers = [](Workspace* workspace) {
		U32 errorCount = 0;
		for (auto i = 0; i < static_cast<int>(workspace->workerCount); i++) {
			errorCount += workspace->workers[i].errorCount;
		}
		return errorCount;
	};

	//template<typename TProcedure, typename... TProcedureArgs>
	//static auto ExecuteProcedureWithAllWorkers = [](Workspace* workspace, TProcedure procedure, TProcedureArgs... args) {
	//	for (auto i = 1; i < workspace->workerCount; i++) {
	//		workspace->threads[i] = std::thread(procedure, &workspace->workers[i + 1], args...);
	//	}
	//	procedure(&workspace->workers[0], args...);
	//}


	// The main thread has finished parsing all work
	JoinAllWorkersInOtherThreads(workspace);
	auto errorCount = GetCurrentErrorCountFromAllWorkers(workspace);
	errorCount += global_workspace.errorCount;
	if (errorCount > 0) {
		printf("There were %d errors parsing the project... exiting", errorCount);
		return;
	}


	LOG_INFO("Analyzing Project");
	//AnalyzeAST(&workspace->workers[0]);	// The main thread resolves its tree

	{
		for (auto i = 1; i < static_cast<int>(workspace->workerCount); i++)
			workspace->threads[i] = std::thread(AnalyzeAST, &workspace->workers[i + 1]);
		AnalyzeAST(&workspace->workers[0]);
		JoinAllWorkersInOtherThreads(workspace);
		auto errorCount = GetCurrentErrorCountFromAllWorkers(workspace);
		errorCount += global_workspace.errorCount;
		if (errorCount > 0) {
			printf("There were %d errors analyzing the project... exiting", errorCount);
			return;
		}
	}

	LOG_INFO("Generating Native Code");
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
