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
      val predicted_pc = Output(UInt(c.XLEN.W))
      val commit = new CommitIO
      val ex = new ExIO
      val mem = new MemIO
      val wb = new WriteBackIO
    }
  })

  val instr = io.in.instr
  val pc = io.in.pc

  val rs1 = instr(19, 15)
  val rs2 = instr(24, 20)
  io.reg_io.raddr(0) := rs1
  io.reg_io.raddr(1) := rs2
  val rop1 = WireInit(io.reg_io.rdata(0))
  val rop2 = WireInit(io.reg_io.rdata(1))

  val decodeInfoList = Lookup(
    instr,
    new DefaultDecodeInfo(pc, instr) { bits.has_error := true.B }.bits.asUInt,
    rv64i.instrSet.map { case (pat, f) => (pat, f(pc, instr).bits.asUInt) }
  )
  val decodeInfo = decodeInfoList.asTypeOf(new DecodeInfoBundle)

  when(decodeInfo.has_error) {
    printf("!!! DECODE ERROR !!!\n");
  }

  io.out.predicted_pc := pc + 4.U

  io.out.commit.pc := pc
  io.out.commit.instr := instr
  io.out.commit.putch := false.B

  io.out.ex.fn := decodeInfo.alufn
  io.out.ex.op32 := false.B
  io.out.ex.is_jump := decodeInfo.is_jump
  io.out.ex.is_branch := decodeInfo.is_branch
  io.out.ex.is_putch := false.B
  io.out.ex.use_imm := decodeInfo.use_imm
  io.out.ex.rs1 := rs1
  io.out.ex.rs2 := rs2
  io.out.ex.op1 := rop1
  io.out.ex.op2 := rop2
  io.out.ex.imm := decodeInfo.imm
  
  io.out.mem := DontCare
  io.out.mem.en := false.B

  io.out.wb.rd := decodeInfo.wb_rd
  io.out.wb.data := DontCare
}
