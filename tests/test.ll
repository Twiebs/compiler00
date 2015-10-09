; ModuleID = 'BangCompiler'

%InternalType = type { i32 }
%InternalType.0 = type { i32, i32, %FirstInternalType }
%FirstInternalType = type { i32 }

define i32 @OtherFunction() {
entry:
  %data = alloca %InternalType
  %access = getelementptr %InternalType, %InternalType* %data, i32 0, i32 0
  store i32 7, i32* %access
  ret i32 0
}

define i32 @main() {
entry:
  %data = alloca %InternalType.0
  %access = getelementptr %InternalType.0, %InternalType.0* %data, i32 0, i32 0
  store i32 5, i32* %access
  ret i32 0
}

declare void @ListAll()

declare void @Build()

declare void @InterpTest()

declare void @printf(i8*, ...)
