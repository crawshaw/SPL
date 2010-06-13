%string = type { i32, [0 x i8]* }
%array = type { i32, [0 x i8*]* }

declare i32 @printf(i8*, ...)
declare i32 @putchar(i32)

define i8* @c_str(%string* %sprintString) {
entry:
  %x1 = getelementptr inbounds %string* %sprintString, i32 0, i32 1
  %x2 = load [0 x i8]** %x1
  %x3 = getelementptr inbounds [0 x i8]* %x2, i32 0, i32 0
  ret i8* %x3
}

define i32 @print(%string* %x1) {
entry:
  %x2 = call i8* (%string*)* @c_str(%string* %x1)
  %calltmp = call i32 (i8*, ...)* @printf(i8* %x2)
  ret i32 %calltmp
}

define i32 @length(%array* %x1) {
  %x2 = getelementptr inbounds %array* %x1, i32 0, i32 0
  %x3 = load i32* %x2
  ret i32 %x3
}
