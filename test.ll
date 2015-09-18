; ModuleID = 'BangCompiler'

@str = private unnamed_addr constant [5 x i8] c"True\00"
@str1 = private unnamed_addr constant [6 x i8] c"False\00"

define i32 @main() {
entry:
  %foo = alloca i32
  store i32 1, i32* %foo
  %0 = load i32* %foo
  %ifcmp = icmp ne i32 %0, 0
  br i1 %ifcmp, label %if, label %else

merge:                                            ; preds = %else, %if
  ret i32 0

if:                                               ; preds = %entry
  call void @Println(i8* getelementptr inbounds ([5 x i8]* @str, i32 0, i32 0))
  br label %merge

else:                                             ; preds = %entry
  call void @Println(i8* getelementptr inbounds ([6 x i8]* @str1, i32 0, i32 0))
  br label %merge
}

define void @Println(i8* %msg) {
entry:
  %msg1 = alloca i8*
  store i8* %msg, i8** %msg1
  %0 = load i8** %msg1
  call void @__PrintlnStr(i8* %0)
  ret void
}

declare void @__PrintlnStr(i8*)

declare void @__PrintInt(i32)

declare void @__PrintFloat(float)

declare void @__PrintStr(i8*)

declare void @__PrintlnInt(i32)

declare void @__PrintlnFloat(float)

define void @Print(i32 %msg) {
entry:
  %msg1 = alloca i32
  store i32 %msg, i32* %msg1
  %0 = load i32* %msg1
  call void @__PrintInt(i32 %0)
  ret void
}

define void @Print2(float %msg) {
entry:
  %msg1 = alloca float
  store float %msg, float* %msg1
  %0 = load float* %msg1
  call void @__PrintFloat(float %0)
  ret void
}

define void @Print3(i8* %msg) {
entry:
  %msg1 = alloca i8*
  store i8* %msg, i8** %msg1
  %0 = load i8** %msg1
  call void @__PrintStr(i8* %0)
  ret void
}

define void @Println4(i32 %msg) {
entry:
  %msg1 = alloca i32
  store i32 %msg, i32* %msg1
  %0 = load i32* %msg1
  call void @__PrintlnInt(i32 %0)
  ret void
}

define void @Println5(float %msg) {
entry:
  %msg1 = alloca float
  store float %msg, float* %msg1
  %0 = load float* %msg1
  call void @__PrintlnFloat(float %0)
  ret void
}
