package rvcore

import chisel3._
import chisel3.util._

class WB(coreConfig: CoreConfig) extends Module {
  val io = IO(new Bundle {
    val stall = Input(Bool())
    val in_valid = Input(Bool())
    val in = new Bundle {
      val pc = Input(UInt(coreConfig.XLEN.W))
      val write_back = new Bundle {
        val rd = Input(UInt(coreConfig.RegAddrWidth.W))
        val data = Input(UInt(coreConfig.XLEN.W))
      }
    }

    val reg_io = new Bundle {
      val wen = Output(Bool())
      val waddr = Output(UInt(coreConfig.RegAddrWidth.W))
      val wdata = Output(UInt(coreConfig.XLEN.W))
    }
  })

  io.reg_io.wen := io.in_valid
  io.reg_io.waddr := io.in.write_back.rd
  io.reg_io.wdata := io.in.write_back.data

  if (coreConfig.DiffTest) {
    val ext = Module(new difftest.DifftestInstrCommit)
    ext.io.clock := clock
    ext.io.coreid := coreConfig.CoreId.U
    ext.io.index := 0.U

    ext.io.valid := io.in_valid && !io.stall
    ext.io.pc := io.in.pc
    ext.io.instr := DontCare // RegNext(io.in.instr)
    ext.io.skip := false.B
    ext.io.isRVC := false.B
    ext.io.scFailed := false.B
    ext.io.wen :=
      io.in_valid && io.in.write_back.rd =/= 0.U
    ext.io.wdata := io.in.write_back.data
    ext.io.wdest := io.in.write_back.rd
  }
}
