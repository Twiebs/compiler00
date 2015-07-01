; ModuleID = 'ir.bc'
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Function Attrs: nounwind uwtable
define i32 @main() #0 {
  %1 = alloca i32, align 4
  %bar = alloca i32, align 4
  %foo = alloca i32, align 4
  store i32 0, i32* %1
  store i32 0, i32* %bar, align 4
  %2 = load i32* %bar, align 4
  %3 = icmp eq i32 %2, 1
  br i1 %3, label %4, label %5

; <label>:4                                       ; preds = %0
  store i32 7, i32* %1
  br label %6

; <label>:5                                       ; preds = %0
  store i32 11, i32* %1
  br label %6

; <label>:6                                       ; preds = %5, %4
  %7 = load i32* %1
  ret i32 %7
}

attributes #0 = { nounwind uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = !{!"clang version 3.6.1 (tags/RELEASE_361/final)"}
