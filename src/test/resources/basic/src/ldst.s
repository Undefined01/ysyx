.include "common.h"

li x1, 1
lw x2, a
mv x3, x2
mv x4, x2
mv x5, x2
lb x6, a
lbu x7, a
lb x8, b
lbu x9, b
la x10, a
lh x11, 0x6(x10)
lh x10, 0x4(x10)
ld x12, a
ld x13, c
addi x12, x12, 8
sd x12, c, x31
lw x14, c
sb x2, b, x31
sh x13, a, x31
ld x15, a
mv x31, x0


.data
.balign 8
a:	.word 0x12345678
b:	.word 0xdeadbeef
c:  .word 0xfeedbabe
d:  .word 0x87654321
