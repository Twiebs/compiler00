; ModuleID = 'BangCompiler'

%Vector2.0 = type { float, float }

define %Vector2.0 @ReturnVector2() {
entry:
  %vector = alloca %Vector2.0
  %access = getelementptr %Vector2.0, %Vector2.0* %vector, i32 0, i32 0
  store float 7.000000e+00, float* %access
  %access1 = getelementptr %Vector2.0, %Vector2.0* %vector, i32 0, i32 0
  store float 2.000000e+00, float* %access1
  %0 = load %Vector2.0, %Vector2.0* %vector
  ret %Vector2.0 %0
}

define i32 @main() {
entry:
  %vector = alloca %Vector2.0
  %calltmp = call %Vector2.0 @ReturnVector2()
  store %Vector2.0 %calltmp, %Vector2.0* %vector
  ret i32 0
}

declare void @printf(i8*, ...)
