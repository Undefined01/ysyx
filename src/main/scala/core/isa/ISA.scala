package rvcore.isa

import chisel3._
import chisel3.util._
import rvcore._
import utils._

class DecodeInfoBundle(implicit c: CoreConfig) extends Bundle {
  val has_error = Bool()

  val use_op1 = Bool()
  val use_op2 = Bool()

  val commit = new CommitIO
  val ex = new ExIO
  val mem = new MemIO
  val wb = new WriteBackIO
}

class DecodeInfo(implicit c: CoreConfig) {
  val bits = Wire(new DecodeInfoBundle)
}

class DefaultDecodeInfo(pc: UInt, instr: UInt)(implicit c: CoreConfig)
    extends DecodeInfo {
  protected val opcode = instr(6, 0)
  protected val funct3 = instr(14, 12)
  protected val funct6 = instr(31, 26)
  protected val funct7 = instr(31, 25)

  protected val rd = instr(11, 7)
  protected val rs1 = instr(19, 15)
  protected val rs2 = instr(24, 20)

  protected val I_imm = SignExt(instr(31, 20), 64)
  protected val S_imm = SignExt(Cat(instr(31, 25), instr(11, 7)), 64)
  protected val B_imm = SignExt(
    Cat(instr(31), instr(7), instr(30, 25), instr(11, 8), 0.U(1.W)),
    64
  )
  protected val U_imm = SignExt(Cat(instr(31, 12), 0.U(12.W)), 64)
  protected val J_imm = SignExt(
    Cat(instr(31), instr(19, 12), instr(20), instr(30, 21), 0.U(1.W)),
    64
  )

  bits.has_error := false.B

  bits.commit.pc := pc
  bits.commit.instr := instr
  bits.commit.is_putch := false.B
  bits.commit.is_csrskip := false.B

  bits.ex := DontCare
  bits.ex.is_op32 := false.B
  bits.ex.is_jump := false.B
  bits.ex.is_branch := false.B
  bits.ex.is_csr := false.B
  bits.ex.is_trap := false.B
  bits.ex.use_imm := false.B
  bits.ex.rs1 := rs1
  bits.ex.rs2 := rs2
  bits.use_op1 := false.B
  bits.use_op2 := false.B

  bits.mem := DontCare
  bits.mem.en := false.B

  bits.wb.rd := 0.U
  bits.wb.data := DontCare
}

abstract class Decoder {
  def apply(pc: UInt, instr: UInt)(implicit c: CoreConfig): DecodeInfo
}

abstract class InstructionSet {
  val instrSet: List[(BitPat, Decoder)]
}
