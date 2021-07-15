package core

import chisel3._
import chisel3.util._
import utils._
import utils.Logger.Debug

class EX_MEM(coreConfig: CoreConfig) extends Module {
  val io = IO(new Bundle {
    val stall = Output(Bool())
    
    val in = new Bundle {
      val valid = Input(Bool())
      val mem = new Bundle {
        val en = Input(Bool())
        val rw = Input(Bool())
        val unsigned = Input(Bool())
        val wWidth = Input(UInt(3.W))
        val addr = Input(UInt(coreConfig.XLEN.W))
        val wdata = Input(UInt(coreConfig.XLEN.W))
      }
      val write_back = new Bundle {
        val rd = Input(UInt(coreConfig.RegAddrWidth.W))
        val data = Input(UInt(coreConfig.XLEN.W))
      }
    }
    val out = new Bundle {
      val valid = Output(Bool())
      val mem = new Bundle {
        val en = Output(Bool())
        val rw = Output(Bool())
        val unsigned = Output(Bool())
        val wWidth = Output(UInt(3.W))
        val addr = Output(UInt(coreConfig.XLEN.W))
        val wdata = Output(UInt(coreConfig.XLEN.W))
      }
      val mem_rdata = Input(UInt(coreConfig.XLEN.W))
      val write_back = new Bundle {
        val rd = Output(UInt(coreConfig.RegAddrWidth.W))
        val data = Output(UInt(coreConfig.XLEN.W))
      }
    }
  })

  val state = RegInit(0.U(1.W))

  io.out.valid := RegEnable(io.in.valid, false.B, io.stall)

  io.out.mem.en := RegEnable(io.in.mem.en, false.B, io.stall)
  io.out.mem.rw := RegEnable(io.in.mem.rw, io.stall)
  io.out.mem.unsigned := RegEnable(io.in.mem.unsigned, io.stall)
  io.out.mem.wWidth := RegEnable(io.in.mem.wWidth, io.stall)
  io.out.mem.addr := RegEnable(io.in.mem.addr, io.stall)
  io.out.mem.wdata := RegEnable(io.in.mem.wdata, io.stall)

  io.out.write_back.rd :=
    RegEnable(Mux(io.in.valid, io.in.write_back.rd, 0.U), 0.U, io.stall)
  io.out.write_back.data := RegEnable(io.in.write_back.data, io.stall)

  io.stall := true.B
  switch(state) {
    is(0.U) {
      when(io.out.mem.en === true.B && io.out.mem.rw === false.B) {
        state := 1.U
        io.stall := false.B
      }
    }
    is(1.U) {
      state := 0.U
      io.out.mem.en := false.B
      io.out.write_back.data := io.out.mem_rdata
    }
  }
}
