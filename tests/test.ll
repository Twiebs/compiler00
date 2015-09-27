; ModuleID = 'BangCompiler'

%Vector3 = type { i32, i32, i32 }

@str = private unnamed_addr constant [13 x i8] c"[%d, %d, %d]\00"
@str2 = private unnamed_addr constant [15 x i8] c"%s[%d, %d, %d]\00"
@str3 = private unnamed_addr constant [9 x i8] c"position\00"

define void @Print(%Vector3* %vector) {
entry:
  %vector1 = alloca %Vector3*
  store %Vector3* %vector, %Vector3** %vector1
  %0 = load %Vector3** %vector1
  %access = getelementptr %Vector3* %0, i32 0, i32 0
  %1 = load i32* %access
  %2 = load %Vector3** %vector1
  %access2 = getelementptr %Vector3* %2, i32 0, i32 1
  %3 = load i32* %access2
  %4 = load %Vector3** %vector1
  %access3 = getelementptr %Vector3* %4, i32 0, i32 2
  %5 = load i32* %access3
  call void (i8*, ...)* @printf(i8* getelementptr inbounds ([13 x i8]* @str, i32 0, i32 0), i32 %1, i32 %3, i32 %5)
  ret void
}

declare void @printf(i8*, ...)

define void @Print1(i8* %name, %Vector3* %vector) {
entry:
  %name1 = alloca i8*
  store i8* %name, i8** %name1
  %vector2 = alloca %Vector3*
  store %Vector3* %vector, %Vector3** %vector2
  %0 = load i8** %name1
  %1 = load %Vector3** %vector2
  %access = getelementptr %Vector3* %1, i32 0, i32 0
  %2 = load i32* %access
  %3 = load %Vector3** %vector2
  %access3 = getelementptr %Vector3* %3, i32 0, i32 1
  %4 = load i32* %access3
  %5 = load %Vector3** %vector2
  %access4 = getelementptr %Vector3* %5, i32 0, i32 2
  %6 = load i32* %access4
  call void (i8*, ...)* @printf(i8* getelementptr inbounds ([15 x i8]* @str2, i32 0, i32 0), i8* %0, i32 %2, i32 %4, i32 %6)
  ret void
}

define i32 @main() {
entry:
  %position = alloca %Vector3
  call void @Print1(i8* getelementptr inbounds ([9 x i8]* @str3, i32 0, i32 0), %Vector3* %position)
  ret i32 0
}

declare void @__PrintInt(i32)

declare void @__PrintFloat(float)

declare void @__PrintStr(i8*)

declare void @__PrintlnInt(i32)

declare void @__PrintlnFloat(float)

declare void @__PrintlnStr(i8*)

define void @Print4(i32 %msg) {
entry:
  %msg1 = alloca i32
  store i32 %msg, i32* %msg1
  %0 = load i32* %msg1
  call void @__PrintInt(i32 %0)
  ret void
}

define void @Print5(float %msg) {
entry:
  %msg1 = alloca float
  store float %msg, float* %msg1
  %0 = load float* %msg1
  call void @__PrintFloat(float %0)
  ret void
}

define void @Print6(i8* %msg) {
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

define void @Println7(float %msg) {
entry:
  %msg1 = alloca float
  store float %msg, float* %msg1
  %0 = load float* %msg1
  call void @__PrintlnFloat(float %0)
  ret void
}

define void @Println8(i8* %msg) {
entry:
  %msg1 = alloca i8*
  store i8* %msg, i8** %msg1
  %0 = load i8** %msg1
  call void @__PrintlnStr(i8* %0)
  ret void
}
