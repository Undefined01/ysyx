package device

import chisel3._
import chisel3.util._
import scala.collection.script.Reset

// Write address channel
class AXI4AWBundle extends Bundle {
  val valid = Output(Bool())
  val ready = Input(Bool())
  val addr = Output(UInt(32.W))
}

// Write data channel
class AXI4WBundle extends Bundle {
  val valid = Output(Bool())
  val ready = Input(Bool())
  val data = Output(UInt(64.W))
  val strb = Output(UInt(8.W))
  val last = Output(Bool())
}

// Write response channel
class AXI4BBundle extends Bundle {
  val valid = Input(Bool())
}

// Read address channel
class AXI4ARBundle extends Bundle {
  val valid = Output(Bool())
  val ready = Input(Bool())
  val addr = Output(UInt(32.W))
  val len = Output(UInt(8.W))
  val size = Output(UInt(3.W))
  val burst = Output(UInt(2.W))
}

// Read data channel
class AXI4RBundle extends Bundle {
  val valid = Input(Bool())
  val data = Input(UInt(64.W))
  val last = Input(Bool())
}

class AXI4Bundle extends Bundle {
  val aw = new AXI4AWBundle
  val w = new AXI4WBundle
  val b = new AXI4BBundle
  val ar = new AXI4ARBundle
  val r = new AXI4RBundle

  def default() = {
    w := DontCare
    ar := DontCare
    r := DontCare
    aw.valid := false.B
    aw.addr := DontCare
    w.valid := false.B
    w.data := DontCare
    w.strb := DontCare
    w.last := DontCare
    ar.valid := false.B
    ar.addr := DontCare
    ar.len := DontCare
    ar.size := DontCare
    ar.burst := DontCare
  }
}

class AXI4RAM extends BlackBox {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())
    val io_in_awready = Output(Bool())
    val io_in_awvalid = Input(Bool())
    val io_in_awaddr = Input(UInt(32.W))
    val io_in_wready = Output(Bool())
    val io_in_wvalid = Input(Bool())
    val io_in_wdata = Input(UInt(64.W))
    val io_in_wstrb = Input(UInt(8.W))
    val io_in_wlast = Input(Bool())
    val io_in_bvalid = Output(Bool())
    val io_in_arready = Output(Bool())
    val io_in_arvalid = Input(Bool())
    val io_in_araddr = Input(UInt(32.W))
    val io_in_arlen = Input(UInt(8.W))
    val io_in_arsize = Input(UInt(3.W))
    val io_in_arburst = Input(UInt(2.W))
    val io_in_rvalid = Output(Bool())
    val io_in_rdata = Output(UInt(64.W))
    val io_in_rlast = Output(Bool())
  })
}

object AXI4RAM {
  def apply(clock: Clock, io: AXI4Bundle) = {
    val mod = Module(new AXI4RAM)
    mod.io.clock := clock
    mod.io.reset := false.B
    io.aw.ready := mod.io.io_in_awready
    mod.io.io_in_awvalid := io.aw.valid
    mod.io.io_in_awaddr := io.aw.addr
    io.w.ready := mod.io.io_in_wready
    mod.io.io_in_wvalid := io.w.valid
    mod.io.io_in_wdata := io.w.data
    mod.io.io_in_wstrb := io.w.strb
    mod.io.io_in_wlast := io.w.last
    io.b.valid := mod.io.io_in_bvalid
    io.ar.ready := mod.io.io_in_arready
    mod.io.io_in_arvalid := io.ar.valid
    mod.io.io_in_araddr := io.ar.addr
    mod.io.io_in_arlen := io.ar.len
    mod.io.io_in_arsize := io.ar.size
    mod.io.io_in_arburst := io.ar.burst
    io.r.valid := mod.io.io_in_rvalid
    io.r.data := mod.io.io_in_rdata
    io.r.last := mod.io.io_in_rlast

    // printf("AXI4RAM ar %x%x %x %x %x %x\n", mod.io.io_in_arready, mod.io.io_in_arvalid, mod.io.io_in_araddr, mod.io.io_in_arlen, mod.io.io_in_arsize, mod.io.io_in_arburst)
    // printf("AXI4RAM r %x %x %x\n", mod.io.io_in_rvalid, mod.io.io_in_rdata, mod.io.io_in_rlast)
  }
}
