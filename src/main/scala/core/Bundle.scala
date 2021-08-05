package rvcore

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._

class ExIO(implicit c: CoreConfig) extends Bundle {
  val fn =UInt(AluFn.bits.W)
  val is_op32 = Bool()
  val is_jump = Bool()
  val is_branch = Bool()
  val is_putch = Bool()
  val use_imm = Bool()
  val rs1 = UInt(c.RegAddrWidth.W)
  val rs2 = UInt(c.RegAddrWidth.W)
  val op1 = UInt(c.XLEN.W)
  val op2 = UInt(c.XLEN.W)
  val imm = UInt(c.XLEN.W)
}

class MemIO(implicit c: CoreConfig) extends Bundle {
  val en = Bool()
  val rw = Bool()
  val unsigned = Bool()
  val wWidth = UInt(3.W)
  val addr = UInt(c.XLEN.W)
  val wdata = UInt(c.XLEN.W)

  def set_valid(valid: Bool) = {
    when(!valid) {
      this.en := false.B
    }
  }
}

class WriteBackIO(implicit c: CoreConfig) extends Bundle {
  val rd = UInt(c.RegAddrWidth.W)
  val data = UInt(c.XLEN.W)

  def set_valid(valid: Bool) = {
    when(!valid) {
      this.rd := 0.U
    }
  }
}

class CommitIO(implicit c: CoreConfig) extends Bundle {
  val pc = UInt(c.XLEN.W)
  val instr = UInt(c.InstrLen.W)
  val putch = UInt(8.W)
}
