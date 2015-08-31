; ModuleID = 'back.cpp'
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@.str = private unnamed_addr constant [18 x i8] c"This is a cstring\00", align 1
@.str1 = private unnamed_addr constant [23 x i8] c"This is another string\00", align 1

; Function Attrs: uwtable
define i32 @main() #0 {
  %c_str = alloca i8*, align 8
  %other_str = alloca i8*, align 8
  store i8* getelementptr inbounds ([18 x i8]* @.str, i32 0, i32 0), i8** %c_str, align 8
  store i8* getelementptr inbounds ([23 x i8]* @.str1, i32 0, i32 0), i8** %other_str, align 8
  %1 = load i8** %c_str, align 8
  %2 = call i32 (i8*, ...)* @printf(i8* %1)
  %3 = load i8** %other_str, align 8
  %4 = call i32 (i8*, ...)* @printf(i8* %3)
  ret i32 0
}

declare i32 @printf(i8*, ...) #1

attributes #0 = { uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = !{!"clang version 3.6.2 (tags/RELEASE_362/final)"}
