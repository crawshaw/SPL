%0 = type { i32, [0 x i8]* }

declare i32 @printf(i8*, ...)

define i32 @print(%0* %x1) {
entry:
  %x2 = getelementptr inbounds %0* %x1, i32 0, i32 1
  %x3 = load [0 x i8]** %x2
  %x4 = getelementptr inbounds [0 x i8]* %x3, i32 0, i32 0
  %calltmp = call i32 (i8*, ...)* @printf(i8* %x4)
  ret i32 %calltmp
}
