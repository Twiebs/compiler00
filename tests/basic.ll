; ModuleID = 'BangCompiler'

%InternalType = type { i32 }
%InternalType.2 = type { i32, i32, %FirstInternalType }
%FirstInternalType = type { i32 }

@str = private unnamed_addr constant [20 x i8] c"Running Cast Test!\0A\00"
@str.1 = private unnamed_addr constant [11 x i8] c"foo is %f\0A\00"
@str.2 = private unnamed_addr constant [13 x i8] c"bar is : %i\0A\00"
@str.3 = private unnamed_addr constant [12 x i8] c"foo is: %f\0A\00"
@str.4 = private unnamed_addr constant [16 x i8] c"someInt is: %d\0A\00"
@str.5 = private unnamed_addr constant [2 x i8] c"\0A\00"
@str.6 = private unnamed_addr constant [23 x i8] c"Running boolean test!\0A\00"
@str.7 = private unnamed_addr constant [23 x i8] c"Foo is greater than 1\0A\00"
@str.8 = private unnamed_addr constant [22 x i8] c"Foo is less than 1.0\0A\00"
@str.9 = private unnamed_addr constant [2 x i8] c"\0A\00"
@str.10 = private unnamed_addr constant [33 x i8] c"InternalStructTestA returned %d\0A\00"
@str.11 = private unnamed_addr constant [33 x i8] c"InternalStructTestB returned %d\0A\00"

define void @CastTest() {
entry:
  call void (i8*, ...) @printf(i8* getelementptr inbounds ([20 x i8], [20 x i8]* @str, i32 0, i32 0))
  %foo = alloca float
  store float 5.000000e+00, float* %foo
  %0 = load float, float* %foo
  call void (i8*, ...) @printf(i8* getelementptr inbounds ([11 x i8], [11 x i8]* @str.1, i32 0, i32 0), float %0)
  %bar = alloca i32
  store i32 8, i32* %bar
  %1 = load i32, i32* %bar
  %2 = sitofp i32 %1 to float
  store float %2, float* %foo
  %3 = load i32, i32* %bar
  call void (i8*, ...) @printf(i8* getelementptr inbounds ([13 x i8], [13 x i8]* @str.2, i32 0, i32 0), i32 %3)
  %4 = load float, float* %foo
  call void (i8*, ...) @printf(i8* getelementptr inbounds ([12 x i8], [12 x i8]* @str.3, i32 0, i32 0), float %4)
  %someInt = alloca i32
  store i32 0, i32* %someInt
  store i32 6, i32* %someInt
  %5 = load i32, i32* %someInt
  call void (i8*, ...) @printf(i8* getelementptr inbounds ([16 x i8], [16 x i8]* @str.4, i32 0, i32 0), i32 %5)
  call void (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @str.5, i32 0, i32 0))
  ret void
}

declare void @printf(i8*, ...)

define void @BooleanTest() {
entry:
  call void (i8*, ...) @printf(i8* getelementptr inbounds ([23 x i8], [23 x i8]* @str.6, i32 0, i32 0))
  %foo = alloca float
  store float 5.000000e-01, float* %foo
  %0 = load float, float* %foo
  %1 = fcmp ogt float %0, 1.000000e+00
  br i1 %1, label %if, label %merge

merge:                                            ; preds = %if, %entry
  %2 = load float, float* %foo
  %3 = fcmp olt float %2, 1.000000e+00
  br i1 %3, label %if2, label %merge1

if:                                               ; preds = %entry
  call void (i8*, ...) @printf(i8* getelementptr inbounds ([23 x i8], [23 x i8]* @str.7, i32 0, i32 0))
  br label %merge

merge1:                                           ; preds = %if2, %merge
  call void (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @str.9, i32 0, i32 0))
  ret void

if2:                                              ; preds = %merge
  call void (i8*, ...) @printf(i8* getelementptr inbounds ([22 x i8], [22 x i8]* @str.8, i32 0, i32 0))
  br label %merge1
}

define i32 @InternalStructTestB() {
entry:
  %data = alloca %InternalType
  %access = getelementptr %InternalType, %InternalType* %data, i32 0, i32 0
  store i32 7, i32* %access
  %access1 = getelementptr %InternalType, %InternalType* %data, i32 0, i32 0
  %0 = load i32, i32* %access1
  ret i32 %0
}

define i32 @InternalStructTestA() {
entry:
  %data = alloca %InternalType.2
  %access = getelementptr %InternalType.2, %InternalType.2* %data, i32 0, i32 0
  store i32 5, i32* %access
  %access1 = getelementptr %InternalType.2, %InternalType.2* %data, i32 0, i32 0
  %0 = load i32, i32* %access1
  ret i32 %0
}

define i32 @main() {
entry:
  call void @CastTest()
  call void @BooleanTest()
  %a = alloca i32
  %calltmp = call i32 @InternalStructTestA()
  store i32 %calltmp, i32* %a
  %b = alloca i32
  %calltmp1 = call i32 @InternalStructTestB()
  store i32 %calltmp1, i32* %b
  %0 = load i32, i32* %a
  call void (i8*, ...) @printf(i8* getelementptr inbounds ([33 x i8], [33 x i8]* @str.10, i32 0, i32 0), i32 %0)
  %1 = load i32, i32* %b
  call void (i8*, ...) @printf(i8* getelementptr inbounds ([33 x i8], [33 x i8]* @str.11, i32 0, i32 0), i32 %1)
  ret i32 0
}

declare void @ListAll()

declare void @Build()

declare void @InterpTest()
