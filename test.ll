; ModuleID = 'BangCompiler'

define i32 @main() {
entry:
  %inferedType = alloca i32
  store i32 5, i32* %inferedType
  %0 = load i32* %inferedType
  %addtmp = add i32 %0, 6
  store i32 %addtmp, i32* %inferedType
  ret i32 0
}
