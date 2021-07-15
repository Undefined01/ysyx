package core

import chisel3._
import chisel3.util._
import utils._
import utils.Logger.Debug

class MEM_WB(coreConfig: CoreConfig) extends Module {
  val io = IO(new Bundle {
    val stall = Input(Bool())

    val in = new Bundle {
      val rdata = Input(UInt(coreConfig.XLEN.W))
      val write_back = new Bundle {
        val rd = Input(UInt(coreConfig.RegAddrWidth.W))
        val data = Input(UInt(coreConfig.XLEN.W))
      }
    }
    
    val out = new Bundle {
      val write_back = new Bundle {
        val rd = Output(UInt(coreConfig.RegAddrWidth.W))
        val data = Output(UInt(coreConfig.XLEN.W))
      }
    }
  })

  io.out.write_back.rd := RegEnable(io.in.write_back.rd, 0.U, io.stall)
  io.out.write_back.data := RegEnable(io.in.write_back.data, io.stall)
}
