package rvcore

import chisel3._
import chisel3.util._

trait CoreConfig {
  val DebugPin = false
  val DiffTest = false
  
  val CoreId = 0
  val XLEN = 64
  val RegReadPorts = 2
  val RegAddrWidth = 5
  val InstrLen = 32
  val InitialPC = BigInt("80000000", 16)
}

class RV64ICoreConfig extends CoreConfig

class RV32ICoreConfig extends CoreConfig {
  override val XLEN = 32
}
