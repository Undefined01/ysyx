package core

import chisel3._
import chisel3.util._

class RegisterFile(coreConfig: CoreConfig) extends Module {
  val io = IO(new Bundle {
    val wen = Input(Bool())
    val waddr = Input(UInt(coreConfig.RegAddrWidth.W))
    val wdata = Input(UInt(coreConfig.XLEN.W))
    val raddr = Input(Vec(coreConfig.RegReadPorts, UInt(coreConfig.RegAddrWidth.W)))
    val rdata = Output(Vec(coreConfig.RegReadPorts, UInt(coreConfig.XLEN.W)))
  })

  val reg = RegInit(VecInit(Seq.fill(32)(0.U(coreConfig.XLEN.W))))

  when(io.wen && io.waddr =/= 0.U) {
    reg(io.waddr) := io.wdata
  }
  for (i <- 0 until coreConfig.RegReadPorts) {
    when(io.raddr(i) === 0.U) {
      io.rdata(i) := 0.U
    }.otherwise {
      io.rdata(i) := reg(io.raddr(i))
    }
  }
}
