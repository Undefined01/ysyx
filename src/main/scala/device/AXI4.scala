package device

import chisel3._
import chisel3.experimental._
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
  val id = UInt(4.W)
  val len = UInt(8.W)
  val size = UInt(3.W)
  val burst = UInt(2.W)
}

// Write data channel
class AXI4WBundle(implicit c: AXI4Config) extends Bundle {
  val data = UInt(c.DataWidth.W)
  val strb = UInt((c.DataWidth / 8).W)
  val last = Bool()
}

// Write response channel
class AXI4BBundle(implicit c: AXI4Config) extends Bundle {
  val resp = UInt(2.W)
  val id = UInt(4.W)
}

// Read address channel
class AXI4ARBundle(implicit c: AXI4Config) extends Bundle {
  val addr = UInt(c.AddrWidth.W)
  val id = UInt(4.W)
  val len = UInt(8.W)
  val size = UInt(3.W)
  val burst = UInt(2.W)
}

// Read data channel
class AXI4RBundle(implicit c: AXI4Config) extends Bundle {
  val resp = UInt(2.W)
  val data = UInt(c.DataWidth.W)
  val last = Bool()
  val id = UInt(4.W)
}

class AXI4Bundle(implicit c: AXI4Config) extends Bundle {
  val aw = Decoupled(Output(new AXI4AWBundle))
  val w = Decoupled(Output(new AXI4WBundle))
  val b = Flipped(Decoupled(Output(new AXI4BBundle)))
  val ar = Decoupled(Output(new AXI4ARBundle))
  val r = Flipped(Decoupled(Output(new AXI4RBundle)))

  def default() = {
    if (
      DataMirror.specifiedDirectionOf(this) == SpecifiedDirection.Unspecified
    ) {
      aw.valid := false.B
      aw.bits := DontCare
      w.valid := false.B
      w.bits := DontCare
      b.ready := false.B
      ar.valid := false.B
      ar.bits := DontCare
      r.ready := false.B
    } else {
      aw.ready := false.B
      w.ready := false.B
      b.valid := false.B
      b.bits := DontCare
      ar.ready := false.B
      r.valid := false.B
      r.bits := DontCare
    }
  }

  def toFlattern(): Data = {
    if (
      DataMirror.specifiedDirectionOf(this) == SpecifiedDirection.Unspecified
    ) {
      val f = Wire(new AXI4Flatten)

      aw.ready := f.awready
      f.awvalid := aw.valid
      f.awaddr := aw.bits.addr
      f.awid := aw.bits.id
      f.awlen := aw.bits.len
      f.awsize := aw.bits.size
      f.awburst := aw.bits.burst

      w.ready := f.wready
      f.wvalid := w.valid
      f.wdata := w.bits.data
      f.wstrb := w.bits.strb
      f.wlast := w.bits.last

      b.valid := f.bvalid
      f.bready := b.ready
      b.bits.resp := f.bresp
      b.bits.id := f.bid

      ar.ready := f.arready
      f.arvalid := ar.valid
      f.araddr := ar.bits.addr
      f.arid := ar.bits.id
      f.arlen := ar.bits.len
      f.arsize := ar.bits.size
      f.arburst := ar.bits.burst

      f.rready := r.ready
      r.valid := f.rvalid
      r.bits.resp := f.rresp
      r.bits.data := f.rdata
      r.bits.last := f.rlast
      r.bits.id := f.rid

      f
    } else {
      val f = Wire(Flipped(new AXI4Flatten))

      f.awready := aw.ready
      aw.valid := f.awvalid
      aw.bits.addr := f.awaddr
      aw.bits.id := f.awid
      aw.bits.len := f.awlen
      aw.bits.size := f.awsize
      aw.bits.burst := f.awburst

      f.wready := w.ready
      w.valid := f.wvalid
      w.bits.data := f.wdata
      w.bits.strb := f.wstrb
      w.bits.last := f.wlast

      f.bvalid := b.valid
      b.ready := f.bready
      f.bresp := b.bits.resp
      f.bid := b.bits.id

      f.arready := ar.ready
      ar.valid := f.arvalid
      ar.bits.addr := f.araddr
      ar.bits.id := f.arid
      ar.bits.len := f.arlen
      ar.bits.size := f.arsize
      ar.bits.burst := f.arburst

      r.ready := f.rready
      f.rvalid := r.valid
      f.rresp := r.bits.resp
      f.rdata := r.bits.data
      f.rlast := r.bits.last
      f.rid := r.bits.id

      f
    }
  }
}

class AXI4Flatten(implicit c: AXI4Config) extends Bundle {
  val awready = Input(Bool())
  val awvalid = Output(Bool())
  val awaddr = Output(UInt(c.AddrWidth.W))
  val awid = Output(UInt(4.W))
  val awlen = Output(UInt(8.W))
  val awsize = Output(UInt(3.W))
  val awburst = Output(UInt(2.W))

  val wready = Input(Bool())
  val wvalid = Output(Bool())
  val wdata = Output(UInt(c.DataWidth.W))
  val wstrb = Output(UInt((c.DataWidth / 8).W))
  val wlast = Output(Bool())

  val bready = Output(Bool())
  val bvalid = Input(Bool())
  val bresp = Input(UInt(2.W))
  val bid = Input(UInt(4.W))

  val arready = Input(Bool())
  val arvalid = Output(Bool())
  val araddr = Output(UInt(c.AddrWidth.W))
  val arid = Output(UInt(4.W))
  val arlen = Output(UInt(8.W))
  val arsize = Output(UInt(3.W))
  val arburst = Output(UInt(2.W))

  val rready = Output(Bool())
  val rvalid = Input(Bool())
  val rresp = Input(UInt(2.W))
  val rdata = Input(UInt(c.DataWidth.W))
  val rlast = Input(Bool())
  val rid = Input(UInt(4.W))
}

class AXI4RAM extends BlackBox with HasBlackBoxResource {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Reset())
    val s_axi = Flipped(new AXI4Flatten()(AXI4Config64))
  })
  addResource("/vsrc/AXI4RAM.v")
}

object AXI4RAM {
  def apply(clock: Clock, reset: Reset, io: AXI4Bundle) = {
    val mod = Module(new AXI4RAM)
    mod.io.clock := clock
    mod.io.reset := reset

    mod.io.s_axi <> io.toFlattern()

    // io.aw.ready := mod.io.io_in_awready
    // mod.io.io_in_awvalid := io.aw.valid
    // mod.io.io_in_awaddr := io.aw.bits.addr

    // io.w.ready := mod.io.io_in_wready
    // mod.io.io_in_wvalid := io.w.valid
    // mod.io.io_in_wdata := io.w.bits.data
    // mod.io.io_in_wstrb := io.w.bits.strb
    // mod.io.io_in_wlast := io.w.bits.last

    // io.b.valid := mod.io.io_in_bvalid
    // mod.io.io_in_bready := io.b.ready

    // io.ar.ready := mod.io.io_in_arready
    // mod.io.io_in_arvalid := io.ar.valid
    // mod.io.io_in_araddr := io.ar.bits.addr
    // mod.io.io_in_arlen := io.ar.bits.len
    // mod.io.io_in_arsize := io.ar.bits.size
    // mod.io.io_in_arburst := io.ar.bits.burst

    // mod.io.io_in_rready := io.r.ready
    // io.r.valid := mod.io.io_in_rvalid
    // io.r.bits.data := mod.io.io_in_rdata
    // io.r.bits.last := mod.io.io_in_rlast

    // printf("AXI4RAM ar %x%x %x %x %x %x\n", mod.io.io_in_arready, mod.io.io_in_arvalid, mod.io.io_in_araddr, mod.io.io_in_arlen, mod.io.io_in_arsize, mod.io.io_in_arburst)
    // printf("AXI4RAM r %x %x %x\n", mod.io.io_in_rvalid, mod.io.io_in_rdata, mod.io.io_in_rlast)
  }
}
