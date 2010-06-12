%0 = type { i32, [0 x i8]* }

declare i32 @printf(i8*, ...)
declare i32 @putchar(i32)

define i8* @c_str(%0* %sprintString) {
entry:
  %x1 = getelementptr inbounds %0* %sprintString, i32 0, i32 1
  %x2 = load [0 x i8]** %x1
  %x3 = getelementptr inbounds [0 x i8]* %x2, i32 0, i32 0
  ret i8* %x3
}

define i32 @print(%0* %x1) {
entry:
  %x2 = call i8* (%0*)* @c_str(%0* %x1)
  %calltmp = call i32 (i8*, ...)* @printf(i8* %x2)
  ret i32 %calltmp
}

; TODO: we can define this in the prelude: print(x); print("\n")
define i32 @println(%0* %x1) {
  %x2 = call i32 (%0*)* @print(%0* %x1)
  %x3 = call i32 (i32)* @putchar(i32 10)
  ret i32 %x3
}
