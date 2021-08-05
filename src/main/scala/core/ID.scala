package rvcore

import chisel3._
import chisel3.util._
import utils._
import rvcore.isa._

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
      val has_error = Output(Bool())
      val predicted_pc = Output(UInt(c.XLEN.W))
      val commit = Output(new CommitIO)
      val ex = Output(new ExIO)
      val mem = Output(new MemIO)
      val wb = Output(new WriteBackIO)
    }
  })

  val instr = io.in.instr
  val pc = io.in.pc

  val decodeInfoList = Lookup(
    instr,
    new DefaultDecodeInfo(pc, instr) { bits.has_error := true.B }.bits.asUInt,
    rv64i.instrSet.map { case (pat, f) => (pat, f(pc, instr).bits.asUInt) }
  )
  val decodeInfo = decodeInfoList.asTypeOf(new DecodeInfoBundle)

  io.reg_io.raddr(0) := decodeInfo.ex.rs1
  io.reg_io.raddr(1) := decodeInfo.ex.rs2
  val rop1 = WireInit(io.reg_io.rdata(0))
  val rop2 = WireInit(io.reg_io.rdata(1))

  io.out.has_error := decodeInfo.has_error
  io.out.predicted_pc := pc + 4.U
  io.out.commit := decodeInfo.commit
  io.out.ex := decodeInfo.ex
  io.out.mem := decodeInfo.mem
  io.out.wb := decodeInfo.wb

  when (!decodeInfo.use_op1) {
    io.out.ex.op1 := rop1
  }
  when (!decodeInfo.use_op2) {
    io.out.ex.op2 := rop2
  }
}
