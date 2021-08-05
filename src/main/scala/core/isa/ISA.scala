package rvcore.isa

import chisel3._
import chisel3.util._
import utils._

class DecodeInfoBundle extends Bundle {
  val has_error = Bool()

  val is_jump = Bool()
  val is_branch = Bool()

  val use_imm = Bool()
  val op1 = UInt(64.W)
  val op2 = UInt(64.W)
  val imm = UInt(64.W)
  val alufn = UInt(64.W)

  val wb_rd = UInt(64.W)
}

class DecodeInfo {
  val bits = Wire(new DecodeInfoBundle)
}

class DefaultDecodeInfo(pc: UInt, instr: UInt) extends DecodeInfo {
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

  bits.is_jump := false.B
  bits.is_branch := false.B

  bits.use_imm := false.B
  bits.op1 := DontCare
  bits.op2 := DontCare
  bits.imm := DontCare
  bits.alufn := DontCare

  bits.wb_rd := 0.U
}

abstract class Decoder {
  def apply(pc: UInt, instr: UInt): DecodeInfo
}

abstract class InstructionSet {
  val instrSet: Array[(BitPat, Decoder)]
}
