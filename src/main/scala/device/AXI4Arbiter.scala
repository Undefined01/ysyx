package device

import chisel3._
import chisel3.util._
import utils.Logger._
import scala.reflect.runtime.universe._
import scala.reflect.ClassTag

// class AXI4Arbiter(
//     masterCnt: Int,
//     slaveAddrSpace: Seq[(UInt, UInt)]
// )(implicit
//     c: AXI4Config
// ) extends Module {
//   private val slaveCnt = slaveAddrSpace.length

//   val io = IO(new Bundle {
//     val masterPort = Vec(masterCnt, new AXI4Bundle)
//     val slavePort = Vec(slaveCnt, new AXI4Bundle)
//   })

//   val rValid = io.masterPort.exists { _.ar.valid }
//   val rRaced = VecInit((1 until masterCnt).scan(false.B) { case (s, x) =>
//     s || io.masterPort(x - 1).ar.valid
//   })
//   val rGrant = VecInit(io.masterPort.zip(rValid).map(_._1.ar.valid && !_._2))

//   val rValidReg = RegNext(rValid)
//   val rGrantReg = RegInit(VecInit(masterCnt, false.B))

//   val rLast = io.masterPort.zip(rGrantReg).map(_._1.r.last && _._2).exists(_)

//   when((rValid && !rValidReg) || rLast) {
//     rGrantReg := rGrant
//   }

// }


class TransInfoBundle extends Bundle {
  var src = Input(UInt(2.W))
  var dst = Input(UInt(2.W))
}

class AXI4Arbiter(implicit c: AXI4Config) extends Module {
  val io = IO(new Bundle {
    val masterPort = Vec(2, Flipped(new AXI4Bundle))
    val slavePort = Vec(1, new AXI4Bundle)
  })

  val addrSpace = Seq(
    (BigInt("80000000", 16), BigInt("90000000", 16))
  )
  val addrSpaceStart = VecInit(addrSpace.map(_._1.U(c.AddrWidth.W)))

  io.masterPort.foreach(_.flippedDefault())
  io.slavePort.foreach(_.default())

  val out = Wire(new AXI4Bundle)
  out.default()

  var rTrans = RegInit(0.U.asTypeOf(Valid(new TransInfoBundle)))
  when(!rTrans.valid) {
    when(io.masterPort(0).ar.valid) {
      rTrans.valid := true.B
      rTrans.bits.src := 0.U
    }.elsewhen(io.masterPort(1).ar.valid) {
      rTrans.valid := true.B
      rTrans.bits.src := 1.U
    }
    rTrans.bits.dst := VecInit(
      addrSpace.map { x =>
        x._1.U <= out.ar.bits.addr && out.ar.bits.addr < x._2.U
      }.zipWithIndex.map { case (x, idx) => Mux(x, idx.U, 0.U) }
    ).reduceTree(_ | _)
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

  val wValidReg = RegInit(false.B)
  val wSourceReg = RegInit(0.U(2.W))
  val wTargetReg = RegInit(0.U(2.W))
  val wLast = out.b.valid

  when(!wValidReg) {
    when(io.masterPort(0).aw.valid) {
      out.aw <> io.masterPort(0).aw
      wValidReg := true.B
      wSourceReg := 0.U
    }.elsewhen(io.masterPort(1).aw.valid) {
      out.aw <> io.masterPort(1).aw
      wValidReg := true.B
      wSourceReg := 1.U
    }
  }.otherwise {
    out.aw <> io.masterPort(wSourceReg).aw
    out.w <> io.masterPort(wSourceReg).w
    out.b <> io.masterPort(wSourceReg).b
    when(wLast) {
      wValidReg := false.B
    }
  }

  when(!wValidReg || wLast) {
    val inAddrSpace = addrSpace.map { x =>
      x._1.U <= out.aw.bits.addr && out.aw.bits.addr < x._2.U
    }
    wTargetReg := VecInit(
      inAddrSpace.zipWithIndex
        .map { case (x, idx) => Mux(x, idx.U, 0.U) }
    ).reduceTree(_ | _)
  }

  io.slavePort(wTargetReg).aw <> out.aw
  io.slavePort(wTargetReg).aw.bits.addr :=
    out.aw.bits.addr - addrSpaceStart(wTargetReg)
  io.slavePort(wTargetReg).w <> out.w
  io.slavePort(wTargetReg).b <> out.b
}

object SignalHolder {
  def apply[T <: Valid[Data]](in: T, clear: Bool, ready: Bool, out: T) = {
    var reg = RegInit(0.U.asTypeOf(Valid(new TransInfoBundle)))
    var readyReg = RegInit(true.B)
    ready := readyReg || clear
    when(in.valid) {
      reg := in
      out := in
      readyReg := false.B
    }.elsewhen(clear) {
      reg.valid := false.B
      out.valid := false.B
      out.bits := DontCare
      readyReg := true.B
    }.otherwise {
      out := reg
    }
  }
}
