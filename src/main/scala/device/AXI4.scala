package device

import chisel3._
import chisel3.util._

trait AXI4Config {
  val AddrWidth: Int
  val DataWidth: Int
}

object AXI4Config64 extends AXI4Config {
  val AddrWidth = 32
  val DataWidth = 64
}

// Write address channel
class AXI4AWBundle(implicit c: AXI4Config) extends Bundle {
  val addr = UInt(c.AddrWidth.W)
}

// Write data channel
class AXI4WBundle(implicit c: AXI4Config) extends Bundle {
  val data = UInt(c.DataWidth.W)
  val strb = UInt((c.DataWidth / 8).W)
  val last = Bool()
}

// Write response channel
class AXI4BBundle(implicit c: AXI4Config) extends Bundle {}

// Read address channel
class AXI4ARBundle(implicit c: AXI4Config) extends Bundle {
  val addr = UInt(c.AddrWidth.W)
  val len = UInt(8.W)
  val size = UInt(3.W)
  val burst = UInt(2.W)
}

// Read data channel
class AXI4RBundle(implicit c: AXI4Config) extends Bundle {
  val data = UInt(64.W)
  val last = Bool()
}

class AXI4Bundle(implicit c: AXI4Config) extends Bundle {
  val aw = Decoupled(Output(new AXI4AWBundle))
  val w = Decoupled(Output(new AXI4WBundle))
  val b = Flipped(Decoupled(Output(new AXI4BBundle)))
  val ar = Decoupled(Output(new AXI4ARBundle))
  val r = Flipped(Decoupled(Output(new AXI4RBundle)))

  def default() = {
    aw.valid := false.B
    aw.bits := DontCare
    w.valid := false.B
    w.bits := DontCare
    b.ready := false.B
    ar.valid := false.B
    ar.bits := DontCare
    r.ready := false.B
  }
  def flippedDefault() = {
    aw.ready := false.B
    w.ready := false.B
    b.valid := false.B
    b.bits := DontCare
    ar.ready := false.B
    r.valid := false.B
    r.bits := DontCare
  }
}

class AXI4RAM extends BlackBox {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Reset())
    val io_in_awready = Output(Bool())
    val io_in_awvalid = Input(Bool())
    val io_in_awaddr = Input(UInt(32.W))
    val io_in_wready = Output(Bool())
    val io_in_wvalid = Input(Bool())
    val io_in_wdata = Input(UInt(64.W))
    val io_in_wstrb = Input(UInt(8.W))
    val io_in_wlast = Input(Bool())
    val io_in_bready = Input(Bool())
    val io_in_bvalid = Output(Bool())
    val io_in_arready = Output(Bool())
    val io_in_arvalid = Input(Bool())
    val io_in_araddr = Input(UInt(32.W))
    val io_in_arlen = Input(UInt(8.W))
    val io_in_arsize = Input(UInt(3.W))
    val io_in_arburst = Input(UInt(2.W))
    val io_in_rready = Input(Bool())
    val io_in_rvalid = Output(Bool())
    val io_in_rdata = Output(UInt(64.W))
    val io_in_rlast = Output(Bool())
  })
}

object AXI4RAM {
  def apply(clock: Clock, reset: Reset, io: AXI4Bundle) = {
    val mod = Module(new AXI4RAM)
    mod.io.clock := clock
    mod.io.reset := reset

    io.aw.ready := mod.io.io_in_awready
    mod.io.io_in_awvalid := io.aw.valid
    mod.io.io_in_awaddr := io.aw.bits.addr

    io.w.ready := mod.io.io_in_wready
    mod.io.io_in_wvalid := io.w.valid
    mod.io.io_in_wdata := io.w.bits.data
    mod.io.io_in_wstrb := io.w.bits.strb
    mod.io.io_in_wlast := io.w.bits.last

    io.b.valid := mod.io.io_in_bvalid
    mod.io.io_in_bready := io.b.ready

    io.ar.ready := mod.io.io_in_arready
    mod.io.io_in_arvalid := io.ar.valid
    mod.io.io_in_araddr := io.ar.bits.addr
    mod.io.io_in_arlen := io.ar.bits.len
    mod.io.io_in_arsize := io.ar.bits.size
    mod.io.io_in_arburst := io.ar.bits.burst

    mod.io.io_in_rready := io.r.ready
    io.r.valid := mod.io.io_in_rvalid
    io.r.bits.data := mod.io.io_in_rdata
    io.r.bits.last := mod.io.io_in_rlast

    // printf("AXI4RAM ar %x%x %x %x %x %x\n", mod.io.io_in_arready, mod.io.io_in_arvalid, mod.io.io_in_araddr, mod.io.io_in_arlen, mod.io.io_in_arsize, mod.io.io_in_arburst)
    // printf("AXI4RAM r %x %x %x\n", mod.io.io_in_rvalid, mod.io.io_in_rdata, mod.io.io_in_rlast)
  }
}
