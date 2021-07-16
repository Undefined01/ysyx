.include "common.h"

auipc x1, 0
auipc x2, 0
auipc x3, 0
auipc x4, 0
xor x2, x2, x1
xor x3, x3, x1
xor x4, x4, x1
lui x1, 0x12345
lui x5, 0x87654
