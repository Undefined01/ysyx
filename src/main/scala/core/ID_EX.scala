package core

import chisel3._
import chisel3.util._
import utils._

class ID_EX(coreConfig: CoreConfig) extends Module {
  val io = IO(new Bundle {
    val stall = Input(Bool())

    val in_valid = Input(Bool())
    val in = new Bundle {
      val predicted_pc = Input(UInt(coreConfig.XLEN.W))
      val ex = new Bundle {
        val fn = Input(UInt(AluFn.bits.W))
        val use_imm = Input(Bool())
        val rs1 = Input(UInt(coreConfig.RegAddrWidth.W))
        val rs2 = Input(UInt(coreConfig.RegAddrWidth.W))
        val op1 = Input(UInt(coreConfig.XLEN.W))
        val op2 = Input(UInt(coreConfig.XLEN.W))
        val imm = Input(UInt(coreConfig.XLEN.W))
      }
      val mem = new Bundle {
        val en = Input(Bool())
        val rw = Input(Bool())
        val unsigned = Input(Bool())
        val wWidth = Input(UInt(3.W))
      }
      val write_back = new Bundle {
        val rd = Input(UInt(coreConfig.RegAddrWidth.W))
      }
    }
    
    val out = new Bundle {
      val valid = Output(Bool())
      val predicted_pc = Output(UInt(coreConfig.XLEN.W))
      val ex = new Bundle {
        val fn = Output(UInt(AluFn.bits.W))
        val use_imm = Output(Bool())
        val rs1 = Output(UInt(coreConfig.RegAddrWidth.W))
        val rs2 = Output(UInt(coreConfig.RegAddrWidth.W))
        val op1 = Output(UInt(coreConfig.XLEN.W))
        val op2 = Output(UInt(coreConfig.XLEN.W))
        val imm = Output(UInt(coreConfig.XLEN.W))
      }
      val mem = new Bundle {
        val en = Output(Bool())
        val rw = Output(Bool())
        val unsigned = Output(Bool())
        val wWidth = Output(UInt(3.W))
      }
      val write_back = new Bundle {
        val rd = Output(UInt(coreConfig.RegAddrWidth.W))
      }
    }
  })

  io.out.valid := RegEnable(io.in_valid, false.B, io.stall)
  io.out.predicted_pc := RegEnable(io.in.predicted_pc, io.stall)
  io.out.ex.fn := RegEnable(io.in.ex.fn, io.stall)
  io.out.ex.use_imm := RegEnable(io.in.ex.use_imm, false.B, io.stall)
  io.out.ex.rs1 := RegEnable(io.in.ex.rs1, io.stall)
  io.out.ex.rs2 := RegEnable(io.in.ex.rs2, io.stall)
  io.out.ex.op1 := RegEnable(io.in.ex.op1, io.stall)
  io.out.ex.op2 := RegEnable(io.in.ex.op2, io.stall)
  io.out.ex.imm := RegEnable(io.in.ex.imm, io.stall)
  io.out.mem.en := RegEnable(io.in.mem.en, io.stall)
  io.out.mem.rw := RegEnable(io.in.mem.rw, io.stall)
  io.out.mem.unsigned := RegEnable(io.in.mem.unsigned, io.stall)
  io.out.mem.wWidth := RegEnable(io.in.mem.wWidth, io.stall)
  io.out.write_back.rd := Mux(
    io.out.valid,
    RegEnable(io.in.write_back.rd, io.stall),
    0.U
  )
}
