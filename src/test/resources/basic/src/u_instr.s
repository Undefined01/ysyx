.include "common.h"

auipc x1, 0
auipc x2, 0
auipc x3, 0
auipc x4, 0
sub x2, x2, x1
sub x3, x3, x1
sub x4, x4, x1
sub x1, x1, x1
lui x5, 0x12345
lui x6, 0x87654
