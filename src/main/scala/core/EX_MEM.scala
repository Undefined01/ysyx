package core

import chisel3._
import chisel3.util._
import utils._
import utils.Logger.Debug

class EX_MEM(coreConfig: CoreConfig) extends Module {
  val io = IO(new Bundle {
    val in_ready = Output(Bool())
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
      val write_back = new Bundle {
        val rd = Output(UInt(coreConfig.RegAddrWidth.W))
        val data = Output(UInt(coreConfig.XLEN.W))
      }
    }
  })

  val state = RegInit(0.U)

  io.out.valid := RegNext(io.in.valid, false.B)

  io.out.mem.en := RegNext(io.in.mem.en, false.B)
  io.out.mem.rw := RegNext(io.in.mem.rw)
  io.out.mem.unsigned := RegNext(io.in.mem.unsigned)
  io.out.mem.wWidth := RegNext(io.in.mem.wWidth)
  io.out.mem.addr := RegNext(io.in.mem.addr)
  io.out.mem.wdata := RegNext(io.in.mem.wdata)

  io.out.write_back.rd :=
    RegNext(Mux(io.in.valid, io.in.write_back.rd, 0.U), 0.U)
  io.out.write_back.data := RegNext(io.in.write_back.data)

  io.in_ready := true.B
  switch(state) {
    is(0.U) {
      when(io.out.mem.en === true.B && io.out.mem.rw === false.B) {
        state := 1.U
        io.in_ready := false.B
      }
    }
    is(1.U) {
      state := 0.U
    }
  }
}
