; ModuleID = 'BangCompiler'

%Vector3 = type { float, float, float }

@str = private unnamed_addr constant [2 x i8] c"[\00"
@str3 = private unnamed_addr constant [3 x i8] c", \00"
@str4 = private unnamed_addr constant [3 x i8] c", \00"
@str5 = private unnamed_addr constant [2 x i8] c"]\00"
@str6 = private unnamed_addr constant [11 x i8] c"position: \00"

define void @Print(%Vector3* %vector) {
entry:
  %vector1 = alloca %Vector3*
  store %Vector3* %vector, %Vector3** %vector1
  call void @Print1(i8* getelementptr inbounds ([2 x i8]* @str, i32 0, i32 0))
  %0 = load %Vector3** %vector1
  %access = getelementptr %Vector3* %0, i32 0, i32 0
  %1 = load float* %access
  call void @Print2(float %1)
  call void @Print1(i8* getelementptr inbounds ([3 x i8]* @str3, i32 0, i32 0))
  %2 = load %Vector3** %vector1
  %access2 = getelementptr %Vector3* %2, i32 0, i32 1
  %3 = load float* %access2
  call void @Print2(float %3)
  call void @Print1(i8* getelementptr inbounds ([3 x i8]* @str4, i32 0, i32 0))
  %4 = load %Vector3** %vector1
  %access3 = getelementptr %Vector3* %4, i32 0, i32 2
  %5 = load float* %access3
  call void @Print2(float %5)
  call void @Print1(i8* getelementptr inbounds ([2 x i8]* @str5, i32 0, i32 0))
  ret void
}

define void @Print1(i8* %msg) {
entry:
  %msg1 = alloca i8*
  store i8* %msg, i8** %msg1
  %0 = load i8** %msg1
  call void @__PrintStr(i8* %0)
  ret void
}

declare void @__PrintStr(i8*)

define void @Print2(float %msg) {
entry:
  %msg1 = alloca float
  store float %msg, float* %msg1
  %0 = load float* %msg1
  call void @__PrintFloat(float %0)
  ret void
}

declare void @__PrintFloat(float)

define i32 @main() {
entry:
  %position = alloca %Vector3
  call void @Print1(i8* getelementptr inbounds ([11 x i8]* @str6, i32 0, i32 0))
  call void @Print(%Vector3* %position)
  %bar = alloca i32
  store i32 7, i32* %bar
  %foo = alloca i32
  %0 = load i32* %bar
  %addtmp = add i32 %0, 6
  %multmp = mul i32 5, %addtmp
  store i32 %multmp, i32* %foo
  %1 = load i32* %foo
  call void @Println(i32 %1)
  ret i32 0
}

define void @Println(i32 %msg) {
entry:
  %msg1 = alloca i32
  store i32 %msg, i32* %msg1
  %0 = load i32* %msg1
  call void @__PrintlnInt(i32 %0)
  ret void
}

declare void @__PrintlnInt(i32)

declare void @__PrintInt(i32)

declare void @__PrintlnFloat(float)

declare void @__PrintlnStr(i8*)

define void @Print7(i32 %msg) {
entry:
  %msg1 = alloca i32
  store i32 %msg, i32* %msg1
  %0 = load i32* %msg1
  call void @__PrintInt(i32 %0)
  ret void
}

define void @Println8(float %msg) {
entry:
  %msg1 = alloca float
  store float %msg, float* %msg1
  %0 = load float* %msg1
  call void @__PrintlnFloat(float %0)
  ret void
}

define void @Println9(i8* %msg) {
entry:
  %msg1 = alloca i8*
  store i8* %msg, i8** %msg1
  %0 = load i8** %msg1
  call void @__PrintlnStr(i8* %0)
  ret void
}
