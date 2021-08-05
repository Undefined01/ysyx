package rvcore.isa

import chisel3._
import chisel3.util._
import utils._

abstract class DecodeInfo {
  val has_error: Bool

  val is_jump: Bool
  val is_branch: Bool

  val use_imm: Bool
  val op1: UInt
  val op2: UInt
  val imm: UInt
  val alufn: UInt

  val wb_rd: UInt

  def toList =
    has_error :: is_jump :: is_branch :: use_imm :: op1 :: op2 :: imm :: alufn :: wb_rd :: Nil
}
object DecodeInfo {
  def fromList[T <: Data](list: List[T]) = {
    val _has_error :: _is_jump :: _is_branch :: _use_imm :: _op1 :: _op2 :: _imm :: _alufn :: _wb_rd :: Nil =
      list
    new DecodeInfo {
      val has_error = _has_error.asInstanceOf[Bool]
      val is_jump = _is_jump.asInstanceOf[Bool]
      val is_branch = _is_branch.asInstanceOf[Bool]
      val use_imm = _use_imm.asInstanceOf[Bool]
      val op1 = _op1.asInstanceOf[UInt]
      val op2 = _op2.asInstanceOf[UInt]
      val imm = _imm.asInstanceOf[UInt]
      val alufn = _alufn.asInstanceOf[UInt]
      val wb_rd = _wb_rd.asInstanceOf[UInt]
    }
  }
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

  val has_error = false.B

  val is_jump = false.B
  val is_branch = false.B

  val use_imm = false.B
  val op1 = DontCare.asUInt
  val op2 = DontCare.asUInt
  val imm = DontCare.asUInt
  val alufn = DontCare.asUInt

  val wb_rd = 0.U
}

abstract class Decoder {
  def apply(pc: UInt, instr: UInt): DecodeInfo
}

abstract class InstructionSet {
  val instrSet: Array[(BitPat, Decoder)]
}
