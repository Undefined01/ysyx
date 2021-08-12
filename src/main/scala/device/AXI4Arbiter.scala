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

  val addrSpace = Seq(
    (BigInt("80000000", 16), BigInt("90000000", 16)),
    (BigInt("a0000000", 16), BigInt("b0000000", 16))
  )
  val addrSpaceStart = VecInit(addrSpace.map(_._1.U(c.AddrWidth.W)))

  io.masterPort.foreach(_.flippedDefault())
  io.slavePort.foreach(_.default())

  val out = Wire(new AXI4Bundle)
  out.default()

  val rValidReg = RegInit(false.B)
  val rSourceReg = RegInit(0.U(2.W))
  val rTargetReg = RegInit(0.U(2.W))

  when(!rValidReg) {
    when(io.masterPort(0).ar.valid) {
      out.ar <> io.masterPort(0).ar
      rValidReg := true.B
      rSourceReg := 0.U
    }.elsewhen(io.masterPort(1).ar.valid) {
      out.ar <> io.masterPort(1).ar
      rValidReg := true.B
      rSourceReg := 1.U
    }
  }.otherwise {
    out.ar <> io.masterPort(rSourceReg).ar
    out.r <> io.masterPort(rSourceReg).r
    when(out.r.valid && out.r.bits.last) {
      rValidReg := false.B
    }
  }

  when(!rValidReg) {
    val inAddrSpace = addrSpace.map { x =>
      x._1.U <= out.ar.bits.addr && out.ar.bits.addr < x._2.U
    }
    rTargetReg := VecInit(
      inAddrSpace.zipWithIndex
        .map { case (x, idx) => Mux(x, idx.U, 0.U) }
    ).reduceTree(_ | _)
  }

  val wValidReg = RegInit(false.B)
  val wSourceReg = RegInit(0.U(2.W))
  val wTargetReg = RegInit(0.U(2.W))

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
    when(out.b.valid) {
      wValidReg := false.B
    }
  }

  when(!wValidReg) {
    val inAddrSpace = addrSpace.map { x =>
      x._1.U <= out.aw.bits.addr && out.aw.bits.addr < x._2.U
    }
    wTargetReg := VecInit(
      inAddrSpace.zipWithIndex
        .map { case (x, idx) => Mux(x, idx.U, 0.U) }
    ).reduceTree(_ | _)
  }

  io.slavePort(rTargetReg).ar <> out.ar
  io.slavePort(rTargetReg).ar.bits.addr :=
    out.ar.bits.addr - addrSpaceStart(rTargetReg)
  io.slavePort(rTargetReg).r <> out.r

  io.slavePort(wTargetReg).aw <> out.aw
  io.slavePort(wTargetReg).aw.bits.addr :=
    out.aw.bits.addr - addrSpaceStart(wTargetReg)
  io.slavePort(wTargetReg).w <> out.w
  io.slavePort(wTargetReg).b <> out.b
}
