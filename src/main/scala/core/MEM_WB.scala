package core

import chisel3._
import chisel3.util._
import utils._
import utils.Logger.Debug

class MEM_WB(coreConfig: CoreConfig) extends Module {
  val io = IO(new Bundle {
    val in = new Bundle {
      val is_mem = Input(Bool())
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

  io.out.write_back.rd := RegNext(io.in.write_back.rd, 0.U)
  val is_mem = RegNext(io.in.is_mem)
  val wb_data = RegNext(io.in.write_back.data)
  io.out.write_back.data := Mux(is_mem, io.in.rdata, wb_data)
}
