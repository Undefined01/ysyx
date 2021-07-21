package rvcore

import chisel3._
import chisel3.util._
import utils._

class ID_EX(implicit coreConfig: CoreConfig) extends Module {
  val io = IO(new Bundle {
    val stall = Input(Bool())

    val in_valid = Input(Bool())
    val in = new Bundle {
      val pc = Input(UInt(coreConfig.XLEN.W))
      val predicted_pc = Input(UInt(coreConfig.XLEN.W))
      val ex = Flipped(new ExIO)
      val mem = Flipped(new MemIO)
      val wb = Flipped(new WriteBackIO)
    }

    val out_valid = Output(Bool())
    val out = new Bundle {
      val pc = Output(UInt(coreConfig.XLEN.W))
      val predicted_pc = Output(UInt(coreConfig.XLEN.W))
      val ex = new ExIO
      val mem = new MemIO
      val wb = new WriteBackIO
    }
  })

  io.out_valid := RegEnable(io.in_valid, false.B, !io.stall)
  io.out.pc := RegEnable(io.in.pc, !io.stall)
  io.out.predicted_pc := RegEnable(io.in.predicted_pc, !io.stall)

  io.out.ex := RegEnable(io.in.ex, !io.stall)

  io.out.mem := RegEnable(io.in.mem, !io.stall)
  io.out.mem.set_valid(io.out_valid)

  io.out.wb := RegEnable(io.in.wb, !io.stall)
  io.out.wb.set_valid(io.out_valid)
}
