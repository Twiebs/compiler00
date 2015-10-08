; ModuleID = 'BangCompiler'

%InternalType = type { i32 }

define i32 @main() {
entry:
  %data = alloca %InternalType
  ret i32 0
}

declare void @ListAll()

declare void @Build()

declare void @InterpTest()
