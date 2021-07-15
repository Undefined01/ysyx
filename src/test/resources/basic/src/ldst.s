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


.data
a:	.word 0x12345678
b:	.word 0xdeadbeef
