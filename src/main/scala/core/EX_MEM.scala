package rvcore

import chisel3._
import chisel3.util._
import utils._
import utils.Logger.Debug

class EX_MEM(implicit c: CoreConfig) extends Module {
  val io = IO(new Bundle {
    val stall = Output(Bool())

    val in_valid = Input(Bool())
    val in = new Bundle {
      val commit = Input(new CommitIO)
      val mem = Input(new MemIO)
      val wb = Input(new WriteBackIO)
    }

    val out_valid = Output(Bool())
    val out = new Bundle {
      val commit = Output(new CommitIO)
      val mem = Output(new MemIO)
      val mem_rdata = Input(UInt(c.XLEN.W))
      val wb = Output(new WriteBackIO)
    }
  })

  val state = RegInit(0.U(1.W))

  io.out_valid := RegEnable(io.in_valid, false.B, !io.stall)
  io.out.commit := RegEnable(io.in.commit, !io.stall)

  io.out.mem := RegEnable(io.in.mem, !io.stall)
  io.out.mem.set_valid(io.out_valid)

  io.out.wb := RegEnable(io.in.wb, !io.stall)
  io.out.wb.set_valid(io.out_valid)

  io.stall := false.B
  switch(state) {
    is(0.U) {
      when(io.out.mem.en === true.B && io.out.mem.rw === false.B) {
        state := 1.U
        io.stall := true.B
      }
    }
    is(1.U) {
      state := 0.U
      io.out.mem.en := false.B
      io.out.wb.data := io.out.mem_rdata
    }
  }
}
