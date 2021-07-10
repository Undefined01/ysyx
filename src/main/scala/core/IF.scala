package core

import chisel3._
import chisel3.util._

class IF(coreConfig: CoreConfig) extends Module {
  val io = IO(new Bundle {
    val in = new Bundle {
      val pc = Input(UInt(coreConfig.XLEN.W))
    }

    // IO to fetch instruction from memory
    val if_io =
      Flipped(new Memory.ReadPort(coreConfig.XLEN, coreConfig.XLEN / 8))

    val out = new Bundle {
      val valid = Output(Bool())
      val pc = Output(UInt(coreConfig.XLEN.W))
      val instr = Output(UInt(coreConfig.InstrLen.W))
    }
  })

  io.if_io.en := true.B
  io.if_io.addr := io.in.pc

  val valid = RegInit(false.B)
  val pc = RegNext(io.in.pc)

  valid := true.B
  io.out.valid := valid
  io.out.pc := pc
  io.out.instr := io.if_io.data
}
