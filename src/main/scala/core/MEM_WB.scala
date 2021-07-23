package rvcore

import chisel3._
import chisel3.util._
import utils._
import utils.Logger.Debug

class MEM_WB(implicit c: CoreConfig) extends Module {
  val io = IO(new Bundle {
    val stall = Input(Bool())

    val in_valid = Input(Bool())
    val in = new Bundle {
      val commit = Flipped(new CommitIO)
      val wb = Flipped(new WriteBackIO)
    }

    val out_valid = Output(Bool())
    val out = new Bundle {
      val commit = new CommitIO
      val wb = new WriteBackIO
    }
  })

  io.out_valid := RegEnable(io.in_valid, false.B, !io.stall)
  io.out.commit := RegEnable(io.in.commit, !io.stall)
  io.out.wb := RegEnable(io.in.wb, !io.stall)
  io.out.wb.set_valid(io.out_valid)
}
