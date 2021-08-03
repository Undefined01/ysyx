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
    io.rport.rdata(i) := reg(io.rport.raddr(i))
  }

  if (c.DebugPort) {
    // val reg_all = WireInit(VecInit.tabulate(32) { x => reg })
    io.debug.get.reg := reg
    // println("Elaborate RegFile!!!!!!!!!!!!!!!\n")
    // BoringUtils.addSource(reg_all, "RegFile_regs")
  }
  if (c.DiffTest) {
    val regState = Module(new difftest.DifftestArchIntRegState)
    regState.io.clock := clock
    regState.io.coreid := c.CoreId.U
    regState.io.gpr := reg

    val a0 = WireInit(reg(10))
    BoringUtils.addSource(a0, "RegFile_a0")
  }
}
