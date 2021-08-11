package device

import chisel3._
import chisel3.util._

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
