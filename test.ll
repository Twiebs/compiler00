; ModuleID = 'BangCompiler'

%Vector3 = type { float, float, float }
%VecRef = type { i32, %Vector3* }

define void @PrintVector(%Vector3* %vector) {
entry:
  %vector1 = alloca %Vector3*
  store %Vector3* %vector, %Vector3** %vector1
  %0 = load %Vector3** %vector1
  %access = getelementptr %Vector3* %0, i32 0, i32 0
  %1 = load float* %access
  call void @Println(float %1)
  %2 = load %Vector3** %vector1
  %access2 = getelementptr %Vector3* %2, i32 0, i32 1
  %3 = load float* %access2
  call void @Println(float %3)
  %4 = load %Vector3** %vector1
  %access3 = getelementptr %Vector3* %4, i32 0, i32 2
  %5 = load float* %access3
  call void @Println(float %5)
  ret void
}

define void @Println(float %msg) {
entry:
  %msg1 = alloca float
  store float %msg, float* %msg1
  %0 = load float* %msg1
  call void @__PrintlnFloat(float %0)
  ret void
}

declare void @__PrintlnFloat(float)

define void @DoStuff(i32 %bool) {
entry:
  %bool1 = alloca i32
  store i32 %bool, i32* %bool1
  br i1 false, label %if, label %else

merge:                                            ; preds = %else, %if
  ret void

if:                                               ; preds = %entry
  call void @Println1(i32 5)
  br label %merge

else:                                             ; preds = %entry
  call void @Println1(i32 76)
  br label %merge
}

define void @Println1(i32 %msg) {
entry:
  %msg1 = alloca i32
  store i32 %msg, i32* %msg1
  %0 = load i32* %msg1
  call void @__PrintlnInt(i32 %0)
  ret void
}

declare void @__PrintlnInt(i32)

define i32 @main() {
entry:
  %foo = alloca i32
  store i32 7, i32* %foo
  br i1 false, label %if, label %else

merge:                                            ; preds = %else, %if
  call void @DoStuff(i32 0)
  %position = alloca %Vector3
  %access = getelementptr %Vector3* %position, i32 0, i32 0
  %access1 = getelementptr %Vector3* %position, i32 0, i32 1
  store float 0x3FF19999A0000000, float* %access1
  %access2 = getelementptr %Vector3* %position, i32 0, i32 2
  store float 0x3FF19999A0000000, float* %access2
  %pos_ptr = alloca %Vector3*
  store %Vector3* %position, %Vector3** %pos_ptr
  %0 = load %Vector3** %pos_ptr
  call void @PrintVector(%Vector3* %0)
  %pos_cpy = alloca %Vector3
  %1 = load %Vector3* %position
  store %Vector3 %1, %Vector3* %pos_cpy
  call void @PrintVector(%Vector3* %pos_cpy)
  %ref = alloca %VecRef
  ret i32 0

if:                                               ; preds = %entry
  call void @Println1(i32 0)
  br label %merge

else:                                             ; preds = %entry
  call void @Println1(i32 1)
  br label %merge
}

declare void @__PrintInt(i32)

declare void @__PrintFloat(float)

declare void @__PrintStr(i8*)

declare void @__PrintlnStr(i8*)

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

define void @Println4(i8* %msg) {
entry:
  %msg1 = alloca i8*
  store i8* %msg, i8** %msg1
  %0 = load i8** %msg1
  call void @__PrintlnStr(i8* %0)
  ret void
}
