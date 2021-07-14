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

    val out_ready = Input(Bool())
    val out = new Bundle {
      val valid = Output(Bool())
      val pc = Output(UInt(coreConfig.XLEN.W))
      val instr = Output(UInt(coreConfig.InstrLen.W))
    }
  })

  io.if_io.en := true.B
  io.if_io.addr := Mux(io.out_ready, io.in.pc >> 3, io.out.pc)
  val instr = Mux(
    io.out.pc(2),
    Cat(io.if_io.data(7), io.if_io.data(6), io.if_io.data(5), io.if_io.data(4)),
    Cat(io.if_io.data(3), io.if_io.data(2), io.if_io.data(1), io.if_io.data(0))
  )

  io.out.valid := RegEnable(true.B, false.B, io.out_ready)
  io.out.pc := RegEnable(io.in.pc, io.out_ready)
  io.out.instr := instr
}
