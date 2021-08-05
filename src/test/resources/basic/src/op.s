.include "common.h"

addi x1, x0, -0x48c
add x2, x1, x1
add x2, x2, x2
addi x3, x0, 4
sra x3, x2, x3
xori x4, x0, 0x148
xor x5, x1, x3
and x6, x4, x3
or x7, x4, x3
xor x8, x3, x4
xor x8, x8, x2
slt x9, x7, x6
sltu x11, x7, x6
addi x10, x2, 0
add x10, x10, x10
add x10, x10, x10
add x10, x10, x10
add x10, x10, x10
sub x11, x10, x5
