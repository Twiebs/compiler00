; ModuleID = 'BangCompiler'

%InternalType = type { i32 }

@str = private unnamed_addr constant [22 x i8] c"foo was clamped to %f\00"
@str.1 = private unnamed_addr constant [22 x i8] c"bar was clamped to %f\00"

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

define void @LambdaInternalTypeTest() {
entry:
  %calltmp = call i32 @DoThings()
  ret void
}

define i32 @DoThings() {
entry:
  %data = alloca %InternalType
  %access = getelementptr %InternalType, %InternalType* %data, i32 0, i32 0
  store i32 7, i32* %access
  %access1 = getelementptr %InternalType, %InternalType* %data, i32 0, i32 0
  %0 = load i32, i32* %access1
  ret i32 %0
}

define i32 @main() {
entry:
  %foo = alloca float
  store float 5.000000e+00, float* %foo
  %0 = load float, float* %foo
  %calltmp = call float @Clamp(float %0, float 6.000000e+00, float 7.000000e+00)
  store float %calltmp, float* %foo
  %1 = load float, float* %foo
  %2 = fpext float %1 to double
  call void (i8*, ...) @printf(i8* getelementptr inbounds ([22 x i8], [22 x i8]* @str, i32 0, i32 0), double %2)
  %bar = alloca float
  store float 0x4022666660000000, float* %bar
  %3 = load float, float* %bar
  %calltmp1 = call float @Clamp(float %3, float 6.000000e+00, float 7.000000e+00)
  store float %calltmp1, float* %bar
  %4 = load float, float* %bar
  %5 = fpext float %4 to double
  call void (i8*, ...) @printf(i8* getelementptr inbounds ([22 x i8], [22 x i8]* @str.1, i32 0, i32 0), double %5)
  call void @LambdaInternalTypeTest()
  ret i32 0
}

declare void @printf(i8*, ...)
