package core

import chisel3._
import chisel3.util._

class IF(coreConfig: CoreConfig) extends Module {
  val io = IO(new Bundle {
    val stall = Input(Bool())
    val flush = Input(Bool())

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
  io.if_io.addr := Mux(!io.stall, io.in.pc >> 3, io.out.pc >> 3)
  val instr = Mux(
    io.out.pc(2),
    Cat(io.if_io.data(7), io.if_io.data(6), io.if_io.data(5), io.if_io.data(4)),
    Cat(io.if_io.data(3), io.if_io.data(2), io.if_io.data(1), io.if_io.data(0))
  )

  io.out.valid :=
    Mux(io.flush, false.B, RegEnable(true.B, false.B, !io.stall))
  io.out.pc := RegEnable(io.in.pc, !io.stall)
  io.out.instr := instr
}
