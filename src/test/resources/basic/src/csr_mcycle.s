.include "common.h"

csrr x1, mcycle
csrr x2, mcycle
csrr x3, mcycle
csrr x4, mcycle
csrr x5, mcycle
sub x2, x2, x1
sub x3, x3, x1
sub x4, x4, x1
sub x5, x5, x1
sub x1, x1, x1
