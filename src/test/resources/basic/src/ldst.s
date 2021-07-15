.include "common.h"

li x1, 1
lw x2, %lo(a)(x0)
mv x3, x2


.data
a:	.word 0x12345678
b:	.word 0x87654321
