package rvcore

import chisel3._
import chisel3.util._
import utils._
import utils.Logger.Debug

class MEM_WB(implicit coreConfig: CoreConfig) extends Module {
  val io = IO(new Bundle {
    val stall = Input(Bool())

    val in_valid = Input(Bool())
    val in = new Bundle {
      val pc = Input(UInt(coreConfig.XLEN.W))
      val wb = Flipped(new WriteBackIO)
    }

    val out_valid = Output(Bool())
    val out = new Bundle {
      val pc = Output(UInt(coreConfig.XLEN.W))
      val wb = new WriteBackIO
    }
  })

  io.out_valid := RegEnable(io.in_valid, false.B, !io.stall)
  io.out.pc := RegEnable(io.in.pc, !io.stall)
  io.out.wb := RegEnable(io.in.wb, !io.stall)
  io.out.wb.set_valid(io.out_valid)
}
