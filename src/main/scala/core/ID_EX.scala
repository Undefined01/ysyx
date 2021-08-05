package rvcore

import chisel3._
import chisel3.util._
import utils._

class ID_EX(implicit c: CoreConfig) extends Module {
  val io = IO(new Bundle {
    val stall = Input(Bool())

    val in_valid = Input(Bool())
    val in = new Bundle {
      val predicted_pc = Input(UInt(c.XLEN.W))
      val commit = Input(new CommitIO)
      val ex = Input(new ExIO)
      val mem = Input(new MemIO)
      val wb = Input(new WriteBackIO)
    }

    val out_valid = Output(Bool())
    val out = new Bundle {
      val predicted_pc = Output(UInt(c.XLEN.W))
      val commit = Output(new CommitIO)
      val ex = Output(new ExIO)
      val mem = Output(new MemIO)
      val wb = Output(new WriteBackIO)
    }
  })

  io.out_valid := RegEnable(io.in_valid, false.B, !io.stall)
  io.out.predicted_pc := RegEnable(io.in.predicted_pc, !io.stall)
  io.out.commit := RegEnable(io.in.commit, !io.stall)

  io.out.ex := RegEnable(io.in.ex, !io.stall)

  io.out.mem := RegEnable(io.in.mem, !io.stall)
  io.out.mem.set_valid(io.out_valid)

  io.out.wb := RegEnable(io.in.wb, !io.stall)
  io.out.wb.set_valid(io.out_valid)
}
