package rvcore

import chisel3._
import chisel3.util._

class IF(implicit c: CoreConfig) extends Module {
  val io = IO(new Bundle {
    val stall = Input(Bool())
    val flush = Input(Bool())

    val in = new Bundle {
      val pc = Input(UInt(c.XLEN.W))
    }

    // IO to fetch instruction from memory
    val if_io = new Bundle {
      val valid = Input(Bool())
      val raddr = Output(UInt(c.XLEN.W))
      val rdata = Input(UInt(c.XLEN.W))
    }

    val out = new Bundle {
      val valid = Output(Bool())
      val pc = Output(UInt(c.XLEN.W))
      val instr = Output(UInt(c.InstrLen.W))
    }
  })

  val is_stall = RegNext(io.stall, false.B)
  val instr = io.if_io.rdata(c.InstrLen - 1, 0)
  val last_instr = RegEnable(instr, !is_stall)

  io.if_io.raddr := Mux(!io.stall, io.in.pc, io.out.pc)

  io.out.valid := !io.flush && RegEnable(io.if_io.valid, false.B, !io.stall)
  io.out.pc := RegEnable(io.in.pc, !io.stall)
  io.out.instr := Mux(is_stall, last_instr, instr)
}
