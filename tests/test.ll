; ModuleID = 'BangCompiler'

@str = private unnamed_addr constant [15 x i8] c"resultA is %d\0A\00"
@str.1 = private unnamed_addr constant [15 x i8] c"resultB is %f\0A\00"
@str.2 = private unnamed_addr constant [15 x i8] c"resultC is %f\0A\00"
@str.3 = private unnamed_addr constant [22 x i8] c"foo was clamped to %f\00"

define float @Clamp(float %value, float %min, float %max) {
entry:
  %value1 = alloca float
  store float %value, float* %value1
  %min2 = alloca float
  store float %min, float* %min2
  %max3 = alloca float
  store float %max, float* %max3
  %0 = load float, float* %value1
  %1 = load float, float* %min2
  %2 = fcmp olt float %0, %1
  br i1 %2, label %if, label %merge

merge:                                            ; preds = %if, %entry
  %3 = load float, float* %value1
  %4 = load float, float* %max3
  %5 = fcmp ogt float %3, %4
  br i1 %5, label %if5, label %merge4

if:                                               ; preds = %entry
  %6 = load float, float* %min2
  store float %6, float* %value1
  br label %merge

merge4:                                           ; preds = %if5, %merge
  %7 = load float, float* %value1
  ret float %7

if5:                                              ; preds = %merge
  %8 = load float, float* %max3
  store float %8, float* %value1
  br label %merge4
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
  call void (i8*, ...) @printf(i8* getelementptr inbounds ([15 x i8], [15 x i8]* @str, i32 0, i32 0), i32 %0)
  %1 = load float, float* %resultB
  %2 = fpext float %1 to double
  call void (i8*, ...) @printf(i8* getelementptr inbounds ([15 x i8], [15 x i8]* @str.1, i32 0, i32 0), double %2)
  %3 = load float, float* %resultC
  %4 = fpext float %3 to double
  call void (i8*, ...) @printf(i8* getelementptr inbounds ([15 x i8], [15 x i8]* @str.2, i32 0, i32 0), double %4)
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

define float @lambdaB(float %a, float %b) {
entry:
  %a1 = alloca float
  store float %a, float* %a1
  %b2 = alloca float
  store float %b, float* %b2
  %temp = alloca float
  %0 = load float, float* %a1
  %1 = load float, float* %b2
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

declare void @printf(i8*, ...)

define i32 @main() {
entry:
  %foo = alloca float
  store float 5.000000e+00, float* %foo
  %0 = load float, float* %foo
  %calltmp = call float @Clamp(float %0, float 6.000000e+00, float 7.000000e+00)
  store float %calltmp, float* %foo
  %1 = load float, float* %foo
  call void (i8*, ...) @printf(i8* getelementptr inbounds ([22 x i8], [22 x i8]* @str.3, i32 0, i32 0), float %1)
  ret i32 0
}
