package core

import chisel3._
import chisel3.util._

trait CoreConfig {
    val XLEN = 64
    val InstrLen = 32
    val InitialPC = 0x0
    val MemoryFile = "test.hex"
    val MemorySize = 40 * 1024
}
