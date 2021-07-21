package rvcore

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

class RegFile(implicit c: CoreConfig) extends Module {
  val io = IO(new Bundle {
    val rport = new Bundle {
      val raddr =
        Input(Vec(c.RegReadPorts, UInt(c.RegAddrWidth.W)))
      val rdata = Output(Vec(c.RegReadPorts, UInt(c.XLEN.W)))
    }
    val wb = Flipped(new WriteBackIO)
    val debug = if (c.DebugPort) Some(new Bundle {
      val reg = Output(Vec(32, UInt(c.XLEN.W)))
    })
    else None
  })

  val negClock = (~clock.asUInt).asBool.asClock
  val reg = withClock(negClock) {
    RegInit(VecInit(Seq.fill(32)(0.U(c.XLEN.W))))
  }

  when(io.wb.rd =/= 0.U) {
    reg(io.wb.rd) := io.wb.data
  }
  for (i <- 0 until c.RegReadPorts) {
    when(io.rport.raddr(i) === 0.U) {
      io.rport.rdata(i) := 0.U
    }.otherwise {
      io.rport.rdata(i) := reg(io.rport.raddr(i))
    }
  }

  if (c.DebugPort) {
    io.debug.get.reg := reg
    // BoringUtils.addSource(reg, "RegFile_Regs")
  }
  if (c.DiffTest) {
    val mod = Module(new difftest.DifftestArchIntRegState)
    mod.io.clock := clock
    mod.io.coreid := c.CoreId.U
    mod.io.gpr := reg
  }
}
