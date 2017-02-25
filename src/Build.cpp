#include "Common.hpp"
#include "Build.hpp"

Compiler g_compiler;

void AnalyzeAST (Worker* worker);

void AddFileToParseQueue(Compiler *compiler, const std::string& filename) {
  compiler->workMutex.lock();
  bool containsFilename = false;
  for (size_t i = 0; i < compiler->addedFiles.size(); i++) {
    SourceFile& file = compiler->addedFiles[i];
    if (Equals(file.name, filename)) {
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


  Compiler *compiler = &g_compiler;

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

    compiler->VoidType  = CreatePrimitiveType("Void", compiler);
    compiler->U8Type    = CreatePrimitiveType("U8", compiler);
    compiler->U16Type   = CreatePrimitiveType("U16", compiler);
    compiler->U32Type   = CreatePrimitiveType("U32", compiler);
    compiler->U64Type   = CreatePrimitiveType("U64", compiler);
    compiler->S8Type    = CreatePrimitiveType("S8", compiler);
    compiler->S16Type   = CreatePrimitiveType("S16", compiler);
    compiler->S32Type   = CreatePrimitiveType("S32", compiler);
    compiler->S64Type   = CreatePrimitiveType("S64", compiler);
    compiler->F16Type   = CreatePrimitiveType("F16", compiler);
    compiler->F32Type   = CreatePrimitiveType("F32", compiler);
    compiler->F64Type   = CreatePrimitiveType("F64", compiler);
    compiler->F128Type  = CreatePrimitiveType("F128", compiler);
  }


  { //Start the parsing process on the provided input file 
    const char *inputFilename = argv[1];
    AddFileToParseQueue(settings.inputFile);
    ThreadProc(compiler, &compiler->workers[0]); 

    //At this point in the compiler all source files for the project have been parsed
    //we now force join all the remaining threads before starting the next phase
    for (int i = 1; i < (int)compiler->workerCount; i++) {
      std::thread& thread = compiler->threads[i - 1];
      thread.join();
    }

    if (compiler->globalErrorCount > 0) {
      printf("There were %u errors parsing the project... exiting", compiler->globalErrorCount);
      return -1;
    }
  }
  
  
	{ //Anaylsis compiler phase
    LOG_INFO("Analyzing Project");
		for (int i = 1; i < (int)compiler->workerCount; i++) {
			compiler->threads[i] = std::thread(AnalyzeAST, &compiler->workers[i + 1]);
    }

		AnalyzeAST(&compiler->workers[0]);

    for (int i = 1; i < (int)compiler->workerCount; i++) {
      std::thread& thread = compiler->threads[i - 1];
      thread.join();
    }

    if (compiler->globalErrorCount > 0) {
      printf("There were %u errors anaylizing the project... exiting", compiler->globalErrorCount);
      return -1;
    }
  }

	LOG_INFO("Generating Native Code");
	CodegenPackage(compiler);
	return 0;
}
