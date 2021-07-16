.include "common.h"

j main

func:
    li      a5,1
.L2:
    beq     a1,a5,.L6
    andi    a4,a1,1
    addi    a0,a0,1
    beqz    a4,.L4
    slli    a4,a1,1
    add     a1,a4,a1
    addi    a1,a1,1
    j       .L2
.L4:
    srai    a1,a1,1
    j       .L2
.L6:
    ret

main:
    mv a0, zero
    li a1, 15
    call func
    mv s2, a0
    mv a0, zero
    li a1, 13
    call func
    mv s3, a0
    mv a0, zero
    li a1, 7
    call func
    mv s4, a0
    li x1, 0x123
