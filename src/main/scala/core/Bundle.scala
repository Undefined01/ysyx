package rvcore

import chisel3._
import chisel3.util._

class WriteBackIO(implicit c: CoreConfig) extends Bundle {
  val rd = Output(UInt(c.RegAddrWidth.W))
  val data = Output(UInt(c.XLEN.W))

  def nop = WireDefault(new Bundle {
    val rd = 0.U
    val data = DontCare
  })
  def set_valid(valid: Bool) = {
    when(!valid) {
      this.rd := 0.U
    }
  }
}
