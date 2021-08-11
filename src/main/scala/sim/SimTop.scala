package sim

import chisel3._
import rvcore._
import device._
import difftest._

class SimTop extends Module {
  val io = IO(new Bundle {
    val logCtrl = new LogCtrlIO
    val perfInfo = new PerfInfoIO
    val uart = new UARTIO
  })

  class RAMHelper extends BlackBox {
    val io = IO(new Bundle {
      val clk = Input(Clock())
      val en = Input(Bool())
      val rIdx = Input(UInt(64.W))
      val rdata = Output(UInt(64.W))
      val wIdx = Input(UInt(64.W))
      val wdata = Input(UInt(64.W))
      val wmask = Input(UInt(64.W))
      val wen = Input(Bool())
    })
  }
  val ram = Module(new RAMHelper)
  val rvcore = Module(
    new RvCore()(
      new RV64ICoreConfig {
        override val DiffTest = true
      },
      AXI4Config64
    )
  )

  ram.io.clk := clock
  ram.io.en := rvcore.io.ram.en
  ram.io.rIdx := rvcore.io.ram.raddr - (BigInt("80000000", 16) >> 3).U
  ram.io.wIdx := rvcore.io.ram.waddr - (BigInt("80000000", 16) >> 3).U
  ram.io.wdata := rvcore.io.ram.wdata
  ram.io.wmask := rvcore.io.ram.wmask
  ram.io.wen := rvcore.io.ram.wen
  rvcore.io.ram.rdata := ram.io.rdata

  io.uart.in.valid := false.B
  io.uart.out.valid := false.B
  io.uart.out.ch := 0.U
}
