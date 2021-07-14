package core

import chisel3._
import chisel3.util._
import utils._

class ID_EX(coreConfig: CoreConfig) extends Module {
  val io = IO(new Bundle {
    val in_ready = Output(Bool())
    val in = new Bundle {
      val valid = Input(Bool())
      val predicted_pc = Input(UInt(coreConfig.XLEN.W))
      val alu = Flipped(new AluInput(coreConfig))
      val mem = new Bundle {
        val en = Input(Bool())
        val rw = Input(Bool())
        val unsigned = Input(Bool())
        val wWidth = Input(UInt(3.W))
        val wdata = Input(UInt(coreConfig.XLEN.W))
      }
      val write_back = new Bundle {
        val rd = Input(UInt(coreConfig.RegAddrWidth.W))
      }
    }
    val out_ready = Input(Bool())
    val out = new Bundle {
      val valid = Output(Bool())
      val predicted_pc = Output(UInt(coreConfig.XLEN.W))
      val alu = new AluInput(coreConfig)
      val mem = new Bundle {
        val en = Output(Bool())
        val rw = Output(Bool())
        val unsigned = Output(Bool())
        val wWidth = Output(UInt(3.W))
        val wdata = Output(UInt(coreConfig.XLEN.W))
      }
      val write_back = new Bundle {
        val rd = Output(UInt(coreConfig.RegAddrWidth.W))
      }
    }
  })

  io.out.valid := RegEnable(io.in.valid, false.B, io.out_ready)
  io.in_ready := io.out_ready
  io.out.predicted_pc := RegEnable(io.in.predicted_pc, io.out_ready)
  io.out.alu := RegEnable(io.in.alu, io.out_ready)
  io.out.mem.en := RegEnable(io.in.mem.en, io.out_ready)
  io.out.mem.rw := RegEnable(io.in.mem.rw, io.out_ready)
  io.out.mem.unsigned := RegEnable(io.in.mem.unsigned, io.out_ready)
  io.out.mem.wWidth := RegEnable(io.in.mem.wWidth, io.out_ready)
  io.out.mem.wdata := RegEnable(io.in.mem.wdata, io.out_ready)
  io.out.write_back.rd := Mux(
    io.out.valid,
    RegEnable(io.in.write_back.rd, io.out_ready),
    0.U
  )
}
