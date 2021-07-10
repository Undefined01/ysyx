package core

import chisel3._
import chisel3.util._

class WB(coreConfig: CoreConfig) extends Module {
  val io = new Bundle {
    val in = new Bundle {
      val valid = Input(Bool())
      val rd = Input(UInt(coreConfig.RegAddrWidth.W))
      val data = Input(UInt(coreConfig.XLEN.W))
    }
    
    val reg_io = new Bundle {
      val wen = Output(Bool())
      val waddr = Output(UInt(coreConfig.RegAddrWidth.W))
      val wdata = Output(UInt(coreConfig.XLEN.W))
    }
  }

  io.reg_io.wen := io.in.valid
  io.reg_io.waddr := io.in.rd
  io.reg_io.wdata := io.in.data
}
