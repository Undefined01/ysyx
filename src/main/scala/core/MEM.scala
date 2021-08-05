package rvcore

import chisel3._
import chisel3.util._
import utils.Logger.Debug

class MEM(implicit c: CoreConfig) extends Module {
  val io = IO(new Bundle {
    val in = new Bundle {
      val mem = Input(new MemIO)
    }
    val mem_io = new Bundle {
      val en = Output(Bool())
      val rw = Output(Bool())
      val unsigned = Output(Bool())
      val wWidth = Output(UInt(3.W))
      val addr = Output(UInt(c.XLEN.W))
      val rdata = Input(UInt(c.XLEN.W))
      val wdata = Output(UInt(c.XLEN.W))
    }
    val out = new Bundle {
      val rdata = Output(UInt(c.XLEN.W))
    }
  })

  io.mem_io.en := io.in.mem.en
  io.mem_io.rw := io.in.mem.rw
  io.mem_io.unsigned := io.in.mem.unsigned
  io.mem_io.wWidth := io.in.mem.wWidth
  io.mem_io.addr := io.in.mem.addr
  io.mem_io.wdata := io.in.mem.wdata

  io.out.rdata := io.mem_io.rdata
}
