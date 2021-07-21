package rvcore

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._

class ExIO(implicit c: CoreConfig) extends Bundle {
  val fn = Output(UInt(AluFn.bits.W))
  val op32 = Output(Bool())
  val is_jump = Output(Bool())
  val is_branch = Output(Bool())
  val use_imm = Output(Bool())
  val rs1 = Output(UInt(c.RegAddrWidth.W))
  val rs2 = Output(UInt(c.RegAddrWidth.W))
  val op1 = Output(UInt(c.XLEN.W))
  val op2 = Output(UInt(c.XLEN.W))
  val imm = Output(UInt(c.XLEN.W))
}

class MemIO(implicit c: CoreConfig) extends Bundle {
  val en = Output(Bool())
  val rw = Output(Bool())
  val unsigned = Output(Bool())
  val wWidth = Output(UInt(3.W))
  val addr = Output(UInt(c.XLEN.W))
  val wdata = Output(UInt(c.XLEN.W))

  def set_valid(valid: Bool) = {
    when(!valid) {
      this.en := false.B
    }
  }
}

class WriteBackIO(implicit c: CoreConfig) extends Bundle {
  val rd = Output(UInt(c.RegAddrWidth.W))
  val data = Output(UInt(c.XLEN.W))

  def set_valid(valid: Bool) = {
    when(!valid) {
      this.rd := 0.U
    }
  }
}
