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

  val axi = Wire(chiselTypeOf(rvcore.io.axi))
  axi <> rvcore.io.axi
  axi.ar.bits.addr := rvcore.io.axi.ar.bits.addr - "h80000000".U
  axi.aw.bits.addr := rvcore.io.axi.aw.bits.addr - "h80000000".U
  AXI4RAM(clock, reset, axi)

  io.uart.in.valid := false.B
  io.uart.out.valid := false.B
  io.uart.out.ch := 0.U
}
