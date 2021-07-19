package rvcore

import chisel3._
import chisel3.util._

trait CoreConfig {
  val DebugPin = false
  val XLEN = 64
  val RegReadPorts = 2
  val RegAddrWidth = 5
  val InstrLen = 32
  val InitialPC = 0x0
  val MemoryFile = ""
  val MemorySize = 40 * 1024
}

class RV64ICoreConfig extends CoreConfig

class RV32ICoreConfig extends CoreConfig {
  override val XLEN = 32
}
