package device

import chisel3._
import chisel3.util._
import utils.Logger._
import scala.reflect.runtime.universe._
import scala.reflect.ClassTag

class TransInfoBundle extends Bundle {
  var src = Input(UInt(2.W))
  var dst = Input(UInt(2.W))
}

class AXI4Arbiter(implicit c: AXI4Config) extends Module {
  val io = IO(new Bundle {
    val masterPort = Vec(2, Flipped(new AXI4Bundle))
    val slavePort = Vec(2, new AXI4Bundle)
    val is_mmio = Output(Bool())
  })

  val addrSpace = Seq(
    (BigInt("00000000", 16), BigInt("9fffffff", 16)),
    (BigInt("02000000", 16), BigInt("0200ffff", 16))
  )
  val addrSpaceStart = VecInit(addrSpace.map(_._1.U(c.AddrWidth.W)))
  def getIdxByAddrSpace(addr: UInt) = {
    VecInit(
      addrSpace
        .map { x =>
          x._1.U <= addr && addr <= x._2.U
        }
        .zipWithIndex
        .map { case (x, idx) => Mux(x, idx.U, 0.U) }
    ).reduceTree(_ | _)
  }

  io.masterPort.foreach(_.default())
  io.slavePort.foreach(_.default())

  val out = Wire(new AXI4Bundle)
  out.default()

  var rTrans = RegInit(0.U.asTypeOf(Valid(new TransInfoBundle)))
  when(!rTrans.valid) {
    when(io.masterPort(0).ar.valid) {
      rTrans.valid := true.B
      rTrans.bits.src := 0.U
      rTrans.bits.dst := getIdxByAddrSpace(io.masterPort(0).ar.bits.addr)
    }.elsewhen(io.masterPort(1).ar.valid) {
      rTrans.valid := true.B
      rTrans.bits.src := 1.U
      rTrans.bits.dst := getIdxByAddrSpace(io.masterPort(1).ar.bits.addr)
    }
  }.otherwise {
    out.ar <> io.masterPort(rTrans.bits.src).ar
    out.r <> io.masterPort(rTrans.bits.src).r
    when(out.r.valid && out.r.ready && out.r.bits.last) {
      rTrans.valid := false.B
    }
  }
  io.slavePort(rTrans.bits.dst).ar <> out.ar
  io.slavePort(rTrans.bits.dst).ar.bits.addr :=
    out.ar.bits.addr - addrSpaceStart(rTrans.bits.dst)
  io.slavePort(rTrans.bits.dst).r <> out.r

  var wTrans = RegInit(0.U.asTypeOf(Valid(new TransInfoBundle)))
  when(!wTrans.valid) {
    when(io.masterPort(0).aw.valid) {
      wTrans.valid := true.B
      wTrans.bits.src := 0.U
    }.elsewhen(io.masterPort(1).aw.valid) {
      wTrans.valid := true.B
      wTrans.bits.src := 1.U
    }
    wTrans.bits.dst := VecInit(
      addrSpace
        .map { x =>
          x._1.U <= out.aw.bits.addr && out.aw.bits.addr <= x._2.U
        }
        .zipWithIndex
        .map { case (x, idx) => Mux(x, idx.U, 0.U) }
    ).reduceTree(_ | _)
  }.otherwise {
    out.aw <> io.masterPort(wTrans.bits.src).aw
    out.w <> io.masterPort(wTrans.bits.src).w
    out.b <> io.masterPort(wTrans.bits.src).b
    when(out.b.valid && out.b.ready) {
      wTrans.valid := false.B
    }
  }
  io.slavePort(wTrans.bits.dst).aw <> out.aw
  io.slavePort(wTrans.bits.dst).aw.bits.addr :=
    out.aw.bits.addr - addrSpaceStart(wTrans.bits.dst)
  io.slavePort(wTrans.bits.dst).w <> out.w
  io.slavePort(wTrans.bits.dst).b <> out.b

  io.is_mmio := (rTrans.valid && rTrans.bits.dst =/= 0.U) || (wTrans.valid && wTrans.bits.dst =/= 0.U)
}
