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
@str.10 = private unnamed_addr constant [15 x i8] c"resultA is %d\0A\00"
@str.11 = private unnamed_addr constant [15 x i8] c"resultB is %f\0A\00"
@str.12 = private unnamed_addr constant [15 x i8] c"resultC is %f\0A\00"
@str.13 = private unnamed_addr constant [31 x i8] c"Running Internal struct tests\0A\00"
@str.14 = private unnamed_addr constant [33 x i8] c"InternalStructTestA returned %d\0A\00"
@str.15 = private unnamed_addr constant [33 x i8] c"InternalStructTestB returned %d\0A\00"
@str.16 = private unnamed_addr constant [22 x i8] c"Running LambdaTest...\00"

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

define void @LambdaTest() {
entry:
  %resultA = alloca i32
  %calltmp = call i32 @lambdaA()
  store i32 %calltmp, i32* %resultA
  %resultB = alloca float
  %calltmp1 = call float @lambdaB(float 2.000000e+00, float 3.000000e+00)
  store float %calltmp1, float* %resultB
  %resultC = alloca float
  %calltmp2 = call float @lambdaC()
  store float %calltmp2, float* %resultC
  %0 = load i32, i32* %resultA
  call void (i8*, ...) @printf(i8* getelementptr inbounds ([15 x i8], [15 x i8]* @str.10, i32 0, i32 0), i32 %0)
  %1 = load float, float* %resultB
  %2 = fpext float %1 to double
  call void (i8*, ...) @printf(i8* getelementptr inbounds ([15 x i8], [15 x i8]* @str.11, i32 0, i32 0), double %2)
  %3 = load float, float* %resultC
  %4 = fpext float %3 to double
  call void (i8*, ...) @printf(i8* getelementptr inbounds ([15 x i8], [15 x i8]* @str.12, i32 0, i32 0), double %4)
  ret void
}

define i32 @lambdaA() {
entry:
  %x = alloca i32
  store i32 5, i32* %x
  %y = alloca i32
  store i32 3, i32* %y
  %z = alloca i32
  %0 = load i32, i32* %x
  %1 = load i32, i32* %y
  %2 = mul i32 %0, %1
  store i32 %2, i32* %z
  %3 = load i32, i32* %z
  ret i32 %3
}

define float @lambdaB(float %"\06", float %"\062") {
entry:
  %"\061" = alloca float
  store float %"\06", float* %"\061"
  %"\063" = alloca float
  store float %"\062", float* %"\063"
  %temp = alloca float
  %0 = load float, float* %"\061"
  %1 = load float, float* %"\063"
  %2 = fmul float %0, %1
  store float %2, float* %temp
  %3 = load float, float* %temp
  ret float %3
}

define float @lambdaC() {
entry:
  %c = alloca i32
  %calltmp = call i32 @lambdaA()
  store i32 %calltmp, i32* %c
  %d = alloca float
  %calltmp1 = call float @lambdaB(float 5.000000e+00, float 2.700000e+01)
  store float %calltmp1, float* %d
  %0 = load i32, i32* %c
  %1 = sitofp i32 %0 to float
  %2 = load float, float* %d
  %3 = fmul float %1, %2
  ret float %3
}

define i32 @main() {
entry:
  call void @CastTest()
  call void @BooleanTest()
  %calltmp = call i32 @RunInternalStructTests()
  call void (i8*, ...) @printf(i8* getelementptr inbounds ([22 x i8], [22 x i8]* @str.16, i32 0, i32 0))
  call void @LambdaTest()
  ret i32 0
}

define i32 @RunInternalStructTests() {
entry:
  call void (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @str.13, i32 0, i32 0))
  %a = alloca i32
  %calltmp = call i32 @InternalStructTestA()
  store i32 %calltmp, i32* %a
  %b = alloca i32
  %calltmp1 = call i32 @InternalStructTestB()
  store i32 %calltmp1, i32* %b
  %0 = load i32, i32* %a
  call void (i8*, ...) @printf(i8* getelementptr inbounds ([33 x i8], [33 x i8]* @str.14, i32 0, i32 0), i32 %0)
  %1 = load i32, i32* %b
  call void (i8*, ...) @printf(i8* getelementptr inbounds ([33 x i8], [33 x i8]* @str.15, i32 0, i32 0), i32 %1)
  ret i32 0
}

declare void @ListAll()

declare void @Build()

declare void @InterpTest()
