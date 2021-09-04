package rvcore

import chisel3._
import chisel3.util._
import utils._
import utils.Logger.Debug

object AluFn {
  val ADD = 0
  val SUB = 0 + 8
  val SLL = 1
  val SLT = 2
  val SLTU = 3
  val XOR = 4
  val SRL = 5
  val SRA = 5 + 8
  val OR = 6
  val AND = 7

  val SEQ = 6 + 8
  val SNE = 7 + 8
  val SGE = 2 + 8
  val SGEU = 3 + 8

  val bits = 4
}

class Alu(implicit c: CoreConfig) extends Module {
  require(c.XLEN == 32 || c.XLEN == 64)
  private val shift_op2_len = c.XLEN match {
    case 32 => 5; case 64 => 6
  }

  val io = IO(new Bundle {
    val in = new Bundle {
      val fn = Input(UInt(AluFn.bits.W))
      val is_op32 = Input(Bool())
      val op1 = Input(UInt(c.XLEN.W))
      val op2 = Input(UInt(c.XLEN.W))
    }
    val out = Output(UInt(c.XLEN.W))
  })

  val fn = io.in.fn
  val is_op32 = io.in.is_op32
  val op1 = io.in.op1
  val op2 = io.in.op2
  val shift_op2 = Mux(is_op32, Cat(0.U(1.W), op2(4, 0)), op2(5, 0))

  val add = op1 + op2
  val sub = op1 - op2
  val sll = op1 << shift_op2
  val lt = op1.asSInt < op2.asSInt
  val ltu = op1 < op2
  val slt = ZeroExt(lt.asUInt, c.XLEN)
  val sltu = ZeroExt(ltu.asUInt, c.XLEN)
  val xor = op1 ^ op2
  val srl_64 = op1 >> shift_op2
  val srl_32 = op1(31, 0) >> shift_op2
  val srl = Mux(is_op32, SignExt(srl_32, c.XLEN), srl_64)
  val sra_64 = (op1.asSInt >> shift_op2).asUInt
  val sra_32 = (op1(31, 0).asSInt >> shift_op2).asUInt
  val sra = Mux(is_op32, SignExt(sra_32, c.XLEN), sra_64)
  val or = op1 | op2
  val and = op1 & op2

  val ne = xor.orR
  val seq = ZeroExt((!ne).asUInt, c.XLEN)
  val sne = ZeroExt(ne.asUInt, c.XLEN)
  val sge = ZeroExt((!lt).asUInt, c.XLEN)
  val sgeu = ZeroExt((!ltu).asUInt, c.XLEN)

  val res = MuxLookup(
    fn,
    add,
    Array(
      AluFn.ADD.U -> add,
      AluFn.SUB.U -> sub,
      AluFn.SLL.U -> sll,
      AluFn.SLT.U -> slt,
      AluFn.SLTU.U -> sltu,
      AluFn.XOR.U -> xor,
      AluFn.SRL.U -> srl,
      AluFn.SRA.U -> sra,
      AluFn.OR.U -> or,
      AluFn.AND.U -> and,
      AluFn.SEQ.U -> seq,
      AluFn.SNE.U -> sne,
      AluFn.SGE.U -> sge,
      AluFn.SGEU.U -> sgeu
    )
  )
  io.out := Mux(is_op32, SignExt(res(31, 0), c.XLEN), res)
}
