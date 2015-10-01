; ModuleID = 'BangCompiler'

define i32 @Test() {
entry:
  ret i32 0
}

define i32 @main() {
entry:
  %x = alloca i32
  store i32 5, i32* %x
  ret i32 0
}
