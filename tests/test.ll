; ModuleID = 'BangCompiler'

@str = private unnamed_addr constant [20 x i8] c"Here is some stuff!\00"

define i32 @Test() {
entry:
  ret i32 0
}

define void @PrintSomeStuff() {
entry:
  call void (i8*, ...)* @printf(i8* getelementptr inbounds ([20 x i8]* @str, i32 0, i32 0))
  ret void
}

declare void @printf(i8*, ...)

declare i32 @Test1(i8)

declare i32 @Test2(i16)

declare i32 @Test3(i32)

declare i32 @Test4(i64)

declare void @Test5(i16*)

declare void @Test6(i8*)

declare void @Test7(i16*)

declare void @Test8(i64*)

define i32 @main() {
entry:
  call void @PrintSomeStuff()
  ret i32 0
}

declare void @ListAll()

declare void @Build()

declare void @InterpTest()
