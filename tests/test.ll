; ModuleID = 'BangCompiler'

@str = private unnamed_addr constant [17 x i8] c"Literal Test: %d\00"

define i32 @main() {
entry:
  call void (i8*, ...)* @printf(i8* getelementptr inbounds ([17 x i8]* @str, i32 0, i32 0), i32 5)
  ret i32 0
}

declare void @printf(i8*, ...)

declare void @__PrintInt(i32)

declare void @__PrintFloat(float)

declare void @__PrintStr(i8*)

declare void @__PrintlnInt(i32)

declare void @__PrintlnFloat(float)

declare void @__PrintlnStr(i8*)

define void @Print(i32 %msg) {
entry:
  %msg1 = alloca i32
  store i32 %msg, i32* %msg1
  %0 = load i32* %msg1
  call void @__PrintInt(i32 %0)
  ret void
}

define void @Print1(float %msg) {
entry:
  %msg1 = alloca float
  store float %msg, float* %msg1
  %0 = load float* %msg1
  call void @__PrintFloat(float %0)
  ret void
}

define void @Print2(i8* %msg) {
entry:
  %msg1 = alloca i8*
  store i8* %msg, i8** %msg1
  %0 = load i8** %msg1
  call void @__PrintStr(i8* %0)
  ret void
}

define void @Println(i32 %msg) {
entry:
  %msg1 = alloca i32
  store i32 %msg, i32* %msg1
  %0 = load i32* %msg1
  call void @__PrintlnInt(i32 %0)
  ret void
}

define void @Println3(float %msg) {
entry:
  %msg1 = alloca float
  store float %msg, float* %msg1
  %0 = load float* %msg1
  call void @__PrintlnFloat(float %0)
  ret void
}

define void @Println4(i8* %msg) {
entry:
  %msg1 = alloca i8*
  store i8* %msg, i8** %msg1
  %0 = load i8** %msg1
  call void @__PrintlnStr(i8* %0)
  ret void
}
