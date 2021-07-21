package rvcore

import chisel3._
import chisel3.util._
import utils._
import utils.Logger.Debug

class EX_MEM(implicit coreConfig: CoreConfig) extends Module {
  val io = IO(new Bundle {
    val stall = Output(Bool())

    val in_valid = Input(Bool())
    val in = new Bundle {
      val pc = Input(UInt(coreConfig.XLEN.W))
      val mem = new Bundle {
        val en = Input(Bool())
        val rw = Input(Bool())
        val unsigned = Input(Bool())
        val wWidth = Input(UInt(3.W))
        val addr = Input(UInt(coreConfig.XLEN.W))
        val wdata = Input(UInt(coreConfig.XLEN.W))
      }
      val wb = Flipped(new WriteBackIO)
    }

    val out_valid = Output(Bool())
    val out = new Bundle {
      val pc = Output(UInt(coreConfig.XLEN.W))
      val mem = new Bundle {
        val en = Output(Bool())
        val rw = Output(Bool())
        val unsigned = Output(Bool())
        val wWidth = Output(UInt(3.W))
        val addr = Output(UInt(coreConfig.XLEN.W))
        val wdata = Output(UInt(coreConfig.XLEN.W))
      }
      val mem_rdata = Input(UInt(coreConfig.XLEN.W))
      val wb = new WriteBackIO
    }
  })

  val state = RegInit(0.U(1.W))

  io.out_valid := RegEnable(io.in_valid, false.B, !io.stall)
  io.out.pc := RegEnable(io.in.pc, !io.stall)

  io.out.mem.en := RegEnable(io.in_valid && io.in.mem.en, false.B, !io.stall)
  io.out.mem.rw := RegEnable(io.in.mem.rw, !io.stall)
  io.out.mem.unsigned := RegEnable(io.in.mem.unsigned, !io.stall)
  io.out.mem.wWidth := RegEnable(io.in.mem.wWidth, !io.stall)
  io.out.mem.addr := RegEnable(io.in.mem.addr, !io.stall)
  io.out.mem.wdata := RegEnable(io.in.mem.wdata, !io.stall)

  io.out.wb.rd := 
    RegEnable(Mux(io.in_valid, io.in.wb.rd, 0.U), 0.U, !io.stall)
  io.out.wb.data := RegEnable(io.in.wb.data, !io.stall)

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
