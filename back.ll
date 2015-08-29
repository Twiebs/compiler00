; ModuleID = 'back.cpp'
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%struct.Vector3 = type { float, float, float }

; Function Attrs: nounwind uwtable
define void @_Z7Printlnf(float %msg) #0 {
  %1 = alloca float, align 4
  store float %msg, float* %1, align 4
  %2 = load float* %1, align 4
  %3 = fadd float %2, 1.000000e+00
  store float %3, float* %1, align 4
  ret void
}

; Function Attrs: nounwind uwtable
define void @_Z7PrintlnP7Vector3(%struct.Vector3* %vector) #0 {
  %1 = alloca %struct.Vector3*, align 8
  store %struct.Vector3* %vector, %struct.Vector3** %1, align 8
  %2 = load %struct.Vector3** %1, align 8
  %3 = getelementptr inbounds %struct.Vector3* %2, i32 0, i32 0
  %4 = load float* %3, align 4
  call void @_Z7Printlnf(float %4)
  %5 = load %struct.Vector3** %1, align 8
  %6 = getelementptr inbounds %struct.Vector3* %5, i32 0, i32 1
  %7 = load float* %6, align 4
  call void @_Z7Printlnf(float %7)
  %8 = load %struct.Vector3** %1, align 8
  %9 = getelementptr inbounds %struct.Vector3* %8, i32 0, i32 2
  %10 = load float* %9, align 4
  call void @_Z7Printlnf(float %10)
  ret void
}

; Function Attrs: nounwind uwtable
define i32 @main() #0 {
  %1 = alloca i32, align 4
  %position = alloca %struct.Vector3, align 4
  store i32 0, i32* %1
  %2 = getelementptr inbounds %struct.Vector3* %position, i32 0, i32 0
  store float 5.000000e+00, float* %2, align 4
  %3 = getelementptr inbounds %struct.Vector3* %position, i32 0, i32 1
  store float 6.000000e+00, float* %3, align 4
  %4 = getelementptr inbounds %struct.Vector3* %position, i32 0, i32 2
  store float 7.000000e+00, float* %4, align 4
  call void @_Z7PrintlnP7Vector3(%struct.Vector3* %position)
  ret i32 0
}

attributes #0 = { nounwind uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = !{!"clang version 3.6.2 (tags/RELEASE_362/final)"}
