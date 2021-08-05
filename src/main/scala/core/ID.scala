package rvcore

import chisel3._
import chisel3.util._
import utils._
import rvcore.isa.DefaultDecodeInfo
import rvcore.isa.rv64i

object DecodeConstant {
  val OpImm = BigInt("0010011", 2)
  val Op = BigInt("0110011", 2)
  val Lui = BigInt("0110111", 2)
  val AuiPc = BigInt("0010111", 2)
  val Load = BigInt("0000011", 2)
  val Store = BigInt("0100011", 2)
  val Jal = BigInt("1101111", 2)
  val Jalr = BigInt("1100111", 2)
  val Branch = BigInt("1100011", 2)

  val OpImm32 = BigInt("0011011", 2)
  val Op32 = BigInt("0111011", 2)

  val Trap = BigInt("1011011", 2)
  val Putch = BigInt("1111011", 2)
}

class ID(implicit c: CoreConfig) extends Module {
  val io = IO(new Bundle {
    val in = new Bundle {
      val pc = Input(UInt(c.XLEN.W))
      val instr = Input(UInt(c.InstrLen.W))
    }

    val reg_io = new Bundle {
      val raddr = Output(Vec(2, UInt(c.RegAddrWidth.W)))
      val rdata = Input(Vec(2, UInt(c.XLEN.W)))
    }

    val out = new Bundle {
      val predicted_pc = Output(UInt(c.XLEN.W))
      val commit = new CommitIO
      val ex = new ExIO
      val mem = new MemIO
      val wb = new WriteBackIO
    }
  })

  val instr = io.in.instr
  val pc = io.in.pc

  val decodeInfoList = ListLookup(
    instr,
    new DefaultDecodeInfo(pc, instr) { override val has_error = true.B }.toList,
    rv64i.instrSet.map { case (pat, f) => (pat, f(pc, instr).toList) }
  )

}
