; ModuleID = 'BangCompiler'

declare void @CreateWindow()

declare void @NewFrame()

define i32 @main() {
entry:
  call void @CreateWindow()
  %i = alloca i32
  store i32 0, i32* %i
  br label %loop

loop:                                             ; preds = %loop, %entry
  call void @NewFrame()
  %0 = load i32* %i
  %1 = sub i32 %0, 1
  store i32 %1, i32* %i
  %2 = load i32* %i
  %increment = add i32 %2, 1
  store i32 %increment, i32* %i
  %3 = load i32* %i
  %loopcond = icmp sle i32 %3, 10
  br i1 %loopcond, label %loop, label %loopexit

loopexit:                                         ; preds = %loop
  ret i32 0
}

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
