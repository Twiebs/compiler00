; ModuleID = 'BangCompiler'

%MemoryBlock.1 = type { i64, i64, i8* }

@str = private unnamed_addr constant [14 x i8] c"Hello World!\0A\00"
@str.1 = private unnamed_addr constant [19 x i8] c"Memblock size: %d\0A\00"
@str.2 = private unnamed_addr constant [19 x i8] c"Memblock used: %d\0A\00"

declare void @printf(i8*, ...)

declare void @ListAll()

declare void @InterpTest()

define i32 @main() {
entry:
  %memblock = alloca %MemoryBlock.1
  %access = getelementptr %MemoryBlock.1, %MemoryBlock.1* %memblock, i32 0, i32 0
  store i64 4096, i64* %access
  %access1 = getelementptr %MemoryBlock.1, %MemoryBlock.1* %memblock, i32 0, i32 1
  store i64 0, i64* %access1
  call void (i8*, ...) @printf(i8* getelementptr inbounds ([14 x i8], [14 x i8]* @str, i32 0, i32 0))
  %access2 = getelementptr %MemoryBlock.1, %MemoryBlock.1* %memblock, i32 0, i32 0
  call void (i8*, ...) @printf(i8* getelementptr inbounds ([19 x i8], [19 x i8]* @str.1, i32 0, i32 0), i64* %access2)
  %access3 = getelementptr %MemoryBlock.1, %MemoryBlock.1* %memblock, i32 0, i32 1
  call void (i8*, ...) @printf(i8* getelementptr inbounds ([19 x i8], [19 x i8]* @str.2, i32 0, i32 0), i64* %access3)
  ret i32 0
}
