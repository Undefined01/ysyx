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

  val rvcore = Module(
    new RvCore()(
      new RV64ICoreConfig {
        override val DiffTest = true
      },
      AXI4Config64
    )
  )

  AXI4RAM(clock, reset, rvcore.io.axi)

  io.uart.in.valid := false.B
  io.uart.out.valid := false.B
  io.uart.out.ch := 0.U
}
