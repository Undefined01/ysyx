package core

import chisel3._
import chisel3.util._

class RegFile(coreConfig: CoreConfig) extends Module {
  val io = IO(new Bundle {
    val rport = new Bundle {
      val raddr =
        Input(Vec(coreConfig.RegReadPorts, UInt(coreConfig.RegAddrWidth.W)))
      val rdata = Output(Vec(coreConfig.RegReadPorts, UInt(coreConfig.XLEN.W)))
    }
    val wport = new Bundle {
      val wen = Input(Bool())
      val waddr = Input(UInt(coreConfig.RegAddrWidth.W))
      val wdata = Input(UInt(coreConfig.XLEN.W))
    }
    val debug = if (coreConfig.DebugPin) Some(new Bundle {
      val reg = Output(Vec(32, UInt(coreConfig.XLEN.W)))
    })
    else None
  })

  val negClock = (~clock.asUInt).asBool.asClock
  val reg = withClock(negClock) {
    RegInit(VecInit(Seq.fill(32)(0.U(coreConfig.XLEN.W))))
  }

  when(io.wport.wen && io.wport.waddr =/= 0.U) {
    reg(io.wport.waddr) := io.wport.wdata
  }
  for (i <- 0 until coreConfig.RegReadPorts) {
    when(io.rport.raddr(i) === 0.U) {
      io.rport.rdata(i) := 0.U
    }.otherwise {
      io.rport.rdata(i) := reg(io.rport.raddr(i))
    }
  }

  if (coreConfig.DebugPin) {
    io.debug.get.reg := reg
  }
}
