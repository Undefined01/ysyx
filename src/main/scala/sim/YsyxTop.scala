package sim

import chisel3._
import rvcore._
import device._
import difftest._

class ysyx_210285 extends Module {
  val io = IO(new Bundle {
    val interrupt = Input(Bool())
    val master = new AXI4Flatten()(AXI4Config64)
    val slave = Flipped(new AXI4Flatten()(AXI4Config64))
  })

  val rvcore = Module(
    new RvCore()(
      new RV64ICoreConfig {
        override val InitialPC = BigInt("30000000", 16)
      },
      AXI4Config64
    )
  )

  io.master <> rvcore.io.axi.toFlattern()
  val slave = Wire(Flipped(new AXI4Bundle()(AXI4Config64)))
  slave.default()
  io.slave <> slave.toFlattern()
}
