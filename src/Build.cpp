#include "Common.hpp"
#include "Build.hpp"

void AnalyzeAST (Worker* worker);

std::ostream& operator<<(std::ostream& stream, const SourceLocation& sourceLocation) {
	stream << "[" << sourceLocation.filename << " " << sourceLocation.lineNumber << ":" << sourceLocation.columnNumber << "]";
	return stream;
}

std::ostream& ReportError() {
	global_workspace.errorCount++;
	return std::cerr;
}

std::ostream& ReportError(const SourceLocation& sourceLocation) {
	global_workspace.errorCount++;
	std::cerr << sourceLocation;
	return std::cerr;
}


static void FreeSubArenas(MemoryArena* arena) {
	if (arena->next != nullptr) {
		FreeSubArenas(arena->next);
		free(arena->next);
	}
}

void AddFileToParseQueue(Compiler *compiler, const std::string& filename) {
  compiler->workMutex.lock();
  bool containsFilename = false;
  for (std::string& str : queue->importedFiles) {
    if (!str.compare(filename)) {
      containsFilename = true;
    }
  }

  if (!containsFilename) {
    queue->fileList.push_back(filename);
    queue->filesToParse.push_back(filename);
    LOG_DEBUG("Added filename: " << filename << " to the global work queue");
  }

  queue->mutex.unlock();
}


static void ParserWorkerThreadProc(Compiler *compiler, Worker *worker) {
	while (compiler->isWorkQueueActive) {
		compiler->workMutex.lock();
    if (compiler->filesToParse.size() > 0) {
      std::string filename = compiler->filesToParse.back();   
      compiler->filesToParse.pop_back();
      compiler->workMutex.unlock();
      ParseFile(worker, filename);
    } else {
      compiler->workMutex.unlock();
    }
  }
}

int main(int argc, char** argv) {
  if (argc < 2) {
    printf("Must provide filename to parse\n");
		return -1;
	}


  Compiler _compiler = {};
  Compiler *compiler = &_compiler;

  { //Initalize the compiler
    compiler->workerCount = FORCE_SINGLE_THREADED ?  1 : std::thread::hardware_concurrency();
    size_t arenaMemorySize = ARENA_BLOCK_SIZE * compiler->workerCount;
    size_t tempMemorySize = workspace->workerCount * TEMP_BLOCK_SIZE;
    size_t requiredMemory = (sizeof(Worker) * compiler->workerCount) + arenaMemorySize + tempMemorySize;
    U8* memory = (U8 *)malloc(requiredMemory);
    memset(memory, 0x00, requiredMemory);
    compiler->workers = (Worker *)memory;

    //Create worker thread structs and set worker-local memory allocators
    U8* arenaMemory = (U8*)(compiler->workers + compiler->workerCount );
    U8* tempMemory = arenaMemory + arenaMemorySize;
    for (size_t i = 0; i < compiler->workerCount ; i++) {
      Worker *worker = &compiler->workers[i];
      worker->workerID = i;
      worker->currentBlock = &compiler->globalBlock;
      worker->tempMemory = tempMemory + (i * TEMP_BLOCK_SIZE);
      worker->arena.memory = arenaMemory + (i * ARENA_BLOCK_SIZE);
      worker->arena.capacity = ARENA_BLOCK_SIZE;
    }

    //Initalize and start the parser work queue
    compiler->isWorkQueueActive = true;
    compiler->threads.resize(workspace->workerCount - 1);
    for (auto i = 1; i < static_cast<int>(workspace->workerCount); i++) {
      Worker *worker = &workspace->workers[i];
      compiler->threads[i - 1] = std::thread(ParserWorkerThreadProc, compiler, worker);
    }

    compiler->buildSettings.emitIR = true;
    compiler->buildSettings.emitNativeOBJ = true;
    compiler->buildSettings.emitExecutable = true;

    Worker *mainWorker = &compiler->workers[0];
    InitalizeLanguagePrimitives(&mainWorker->arena, &compiler->globalBlock);
  }


  { //Start the parsing process on the provided input file 
    const char *inputFilename = argv[1];
    AddFileToParseQueue(settings.inputFile);
    ThreadProc(&workspace->workers[0], &workspace->workQueue, 0, &settings);

    //At this point in the compiler all source files for the project have been parsed
    //we now force join all the remaining threads before starting the next phase
    for (int i = 1; i < (int)workspace->workerCount; i++) {
      std::thread& thread = workspace->threads[i - 1];
      thread.join();
    }

    if (compiler->globalErrorCount > 0) {
      printf("There were %u errors parsing the project... exiting", compiler->globalErrorCount);
      return -1;
    }
  }
  
  
	{ //Anaylsis compiler phase
    LOG_INFO("Analyzing Project");
		for (int i = 1; i < (int)workspace->workerCount; i++) {
			workspace->threads[i] = std::thread(AnalyzeAST, &workspace->workers[i + 1]);
    }

		AnalyzeAST(&workspace->workers[0]);

    for (int i = 1; i < (int)workspace->workerCount; i++) {
      std::thread& thread = workspace->threads[i - 1];
      thread.join();
    }

    if (compiler->globalErrorCount > 0) {
      printf("There were %u errors anaylizing the project... exiting", compiler->globalErrorCount);
      return -1;
    }
  }

	LOG_INFO("Generating Native Code");
	CodegenPackage(package, &global_settings);

  FreeSubArenas(&package->arena);
	return 0;
}
