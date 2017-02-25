#include <stdio.h>

void ReportSourceError(const SourceLocation& location, const char *fmt, ...) {
  Compiler *compiler = &g_compiler;
  compiler->errorMutex.lock();
  va_list args;
  va_start(args, fmt);
  SourceFile& file = compiler->addedFiles[location.fileID];
  printf("[%.*s %u:%u]", file.path.length, file.path.string, (U32)file.lineNumber, (U32)file.columnNumber);
  vprintf(fmt, args);
  printf("\n");
  compiler->errorMutex.unlock();
}

void ReportError(const char *fmt, ...) {
  Compiler *compiler = &g_compiler;
  compiler->errorMutex.lock();
  va_list args;
  va_start(args, fmt); 
  vprintf(fmt, args);
  printf("\n");
  compiler->errorMutex.unlock();
}