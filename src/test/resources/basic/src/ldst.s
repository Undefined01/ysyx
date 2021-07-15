.include "common.h"

li x1, 1
lw x2, %lo(a)(x0)
mv x3, x2
mv x4, x2
mv x5, x2
lb x6, %lo(a)(x0)
lbu x7, %lo(a)(x0)
lb x8, %lo(b)(x0)
lbu x9, %lo(b)(x0)
li x10, %lo(a)
lh x11, 0x6(x10)
lh x10, 0x4(x10)
ld x12, %lo(a)(x0)
ld x13, %lo(c)(x0)
addi x12, x12, 8
sd x12, %lo(c)(x0)
lw x14, %lo(c)(x0)
sb x2, %lo(b)(x0)
sh x13, %lo(a)(x0)
ld x15, %lo(a)(x0)


.data
.balign 8
a:	.word 0x12345678
b:	.word 0xdeadbeef
c:  .word 0xfeedbabe
d:  .word 0x87654321
