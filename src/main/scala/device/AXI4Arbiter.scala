package device

import chisel3._
import chisel3.util._
import utils.Logger._

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

class AXI4Arbiter(implicit c: AXI4Config) extends Module {
  val io = IO(new Bundle {
    val masterPort = Vec(2, Flipped(new AXI4Bundle))
    val slavePort = Vec(1, new AXI4Bundle)
  })

  io.masterPort.foreach(_.flippedDefault())
  io.slavePort.foreach(_.default())

  val out = Wire(new AXI4Bundle)
  out.default()

  val rValidReg = RegInit(false.B)
  val rCurReg = RegInit(0.U(2.W))

  when(!rValidReg) {
    when(io.masterPort(0).ar.valid) {
      out.ar <> io.masterPort(0).ar
      rValidReg := true.B
      rCurReg := 0.U
    }.elsewhen(io.masterPort(1).ar.valid) {
      out.ar <> io.masterPort(1).ar
      rValidReg := true.B
      rCurReg := 1.U
    }
  }.otherwise {
    out.ar <> io.masterPort(rCurReg).ar
    out.r <> io.masterPort(rCurReg).r
    when(out.r.valid && out.r.bits.last) {
      rValidReg := false.B
    }
  }

  val wValidReg = RegInit(false.B)
  val wCurReg = RegInit(0.U(2.W))

  when(!wValidReg) {
    when(io.masterPort(0).aw.valid) {
      out.aw <> io.masterPort(0).aw
      wValidReg := true.B
      wCurReg := 0.U
    }.elsewhen(io.masterPort(1).aw.valid) {
      out.aw <> io.masterPort(1).aw
      wValidReg := true.B
      wCurReg := 1.U
    }
  }.otherwise {
    out.aw <> io.masterPort(wCurReg).aw
    out.w <> io.masterPort(wCurReg).w
    out.b <> io.masterPort(wCurReg).b
    when(out.b.valid) {
      wValidReg := false.B
    }
  }

  io.slavePort(0) <> out
}
