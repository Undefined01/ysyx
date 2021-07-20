package rvcore

import chisel3._
import chisel3.util._
import utils._
import utils.Logger.Debug

class MEM_WB(coreConfig: CoreConfig) extends Module {
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

    val out_valid = Output(Bool())
    val out = new Bundle {
      val pc = Output(UInt(coreConfig.XLEN.W))
      val write_back = new Bundle {
        val rd = Output(UInt(coreConfig.RegAddrWidth.W))
        val data = Output(UInt(coreConfig.XLEN.W))
      }
    }
  })

  io.out_valid := RegEnable(io.in_valid, false.B, !io.stall)
  io.out.pc := RegEnable(io.in.pc, !io.stall)
  io.out.write_back.rd := RegEnable(io.in.write_back.rd, 0.U, !io.stall)
  io.out.write_back.data := RegEnable(io.in.write_back.data, !io.stall)
}
