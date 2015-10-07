; ModuleID = 'BangCompiler'

%Package = type { i32 }

@str = private unnamed_addr constant [16 x i8] c"Package id: %d\0A\00"
@str1 = private unnamed_addr constant [11 x i8] c"foo is %f\0A\00"
@str2 = private unnamed_addr constant [13 x i8] c"bar is : %i\0A\00"
@str3 = private unnamed_addr constant [12 x i8] c"foo is: %f\0A\00"
@str4 = private unnamed_addr constant [16 x i8] c"someInt is: %d\0A\00"
@str5 = private unnamed_addr constant [23 x i8] c"Foo is greater than 1\0A\00"
@str6 = private unnamed_addr constant [22 x i8] c"Foo is less than 1.0\0A\00"

define void @CastTest() {
entry:
  %package = alloca %Package
  %access = getelementptr %Package* %package, i32 0, i32 0
  store i32 3, i32* %access
  %access1 = getelementptr %Package* %package, i32 0, i32 0
  %0 = load i32* %access1
  call void (i8*, ...)* @printf(i8* getelementptr inbounds ([16 x i8]* @str, i32 0, i32 0), i32 %0)
  %foo = alloca float
  store float 5.000000e+00, float* %foo
  %1 = load float* %foo
  call void (i8*, ...)* @printf(i8* getelementptr inbounds ([11 x i8]* @str1, i32 0, i32 0), float %1)
  %bar = alloca i32
  store i32 8, i32* %bar
  %2 = load i32* %bar
  %3 = sitofp i32 %2 to float
  store float %3, float* %foo
  %4 = load i32* %bar
  call void (i8*, ...)* @printf(i8* getelementptr inbounds ([13 x i8]* @str2, i32 0, i32 0), i32 %4)
  %5 = load float* %foo
  call void (i8*, ...)* @printf(i8* getelementptr inbounds ([12 x i8]* @str3, i32 0, i32 0), float %5)
  %someInt = alloca i32
  store i32 0, i32* %someInt
  store i32 6, i32* %someInt
  %6 = load i32* %someInt
  call void (i8*, ...)* @printf(i8* getelementptr inbounds ([16 x i8]* @str4, i32 0, i32 0), i32 %6)
  ret void
}

declare void @printf(i8*, ...)

define void @BooleanTest() {
entry:
  %foo = alloca float
  store float 0x4014CCCCC0000000, float* %foo
  %0 = load float* %foo
  %1 = fcmp ogt float %0, 1.000000e+00
  br i1 %1, label %if, label %merge

merge:                                            ; preds = %if, %entry
  %2 = load float* %foo
  %3 = fcmp olt float %2, 1.000000e+00
  br i1 %3, label %if2, label %merge1

if:                                               ; preds = %entry
  call void (i8*, ...)* @printf(i8* getelementptr inbounds ([23 x i8]* @str5, i32 0, i32 0))
  br label %merge

merge1:                                           ; preds = %if2, %merge
  ret void

if2:                                              ; preds = %merge
  call void (i8*, ...)* @printf(i8* getelementptr inbounds ([22 x i8]* @str6, i32 0, i32 0))
  br label %merge1
}

define i32 @main() {
entry:
  call void @CastTest()
  call void @BooleanTest()
  ret i32 0
}

declare void @ListAll()

declare void @Build()

declare void @InterpTest()
