.include "common.h"

li a0, 10
call func
mv x15, a0
li x1, %lo(func)
li a0, 20
jalr x1

li x1, 0x5f7

j end

func:
    add a0, a0, a0
    ret

end:
