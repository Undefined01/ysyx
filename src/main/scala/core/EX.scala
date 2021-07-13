package core

import chisel3._
import chisel3.util._
import utils._

object AluFn extends Enumeration {
  val ADD, SUB, SLL, SLT, SLTU, XOR, SRL, SRA, OR, AND = Value
  val SEQ, SNE, SGE, SGEU = Value

  val bits = 4
}

class AluInput(coreConfig: CoreConfig) extends Bundle {
  val fn = Output(UInt(AluFn.bits.W))
  val op1 = Output(UInt(coreConfig.XLEN.W))
  val op2 = Output(UInt(coreConfig.XLEN.W))
}

class Alu(coreConfig: CoreConfig) extends Module {
  require(coreConfig.XLEN == 32 || coreConfig.XLEN == 64)
  private val shift_op2_len = coreConfig.XLEN match {
    case 32 => 5; case 64 => 6
  }
  val io = IO(new Bundle {
    val in = Flipped(new AluInput(coreConfig))
    val out = Output(UInt(coreConfig.XLEN.W))
  })

  val fn = io.in.fn
  val op1 = io.in.op1
  val op2 = io.in.op2

  val add_sub_op2 = if (fn == AluFn.SUB) ~op2 + 1.U else op2
  val add_sub = op1 + add_sub_op2

  val sll = op1 << op2(shift_op2_len - 1, 0)
  val lt = op1.asSInt < op2.asSInt
  val ltu = op1 < op2
  val slt = ZeroExt(lt.asUInt, coreConfig.XLEN)
  val sltu = ZeroExt(ltu.asUInt, coreConfig.XLEN)
  val xor = op1 ^ op2
  val srl = op1 >> op2(shift_op2_len - 1, 0)
  val sra = (op1.asSInt >> op2(shift_op2_len - 1, 0)).asUInt
  val or = op1 | op2
  val and = op1 & op2

  val ne = xor.orR
  val seq = ZeroExt((!ne).asUInt, coreConfig.XLEN)
  val sne = ZeroExt(ne.asUInt, coreConfig.XLEN)
  val sge = ZeroExt((!lt).asUInt, coreConfig.XLEN)
  val sgeu = ZeroExt((!ltu).asUInt, coreConfig.XLEN)

  io.out := MuxLookup(
    fn,
    add_sub,
    Array(
      AluFn.ADD.id.U -> add_sub,
      AluFn.SUB.id.U -> add_sub,
      AluFn.SLL.id.U -> sll,
      AluFn.SLT.id.U -> slt,
      AluFn.SLTU.id.U -> sltu,
      AluFn.XOR.id.U -> xor,
      AluFn.SRL.id.U -> srl,
      AluFn.SRA.id.U -> sra,
      AluFn.OR.id.U -> or,
      AluFn.AND.id.U -> and,
      AluFn.SEQ.id.U -> seq,
      AluFn.SNE.id.U -> sne,
      AluFn.SGE.id.U -> sge,
      AluFn.SGEU.id.U -> sgeu
    )
  )
}

class EX(coreConfig: CoreConfig) extends Module {
  val io = IO(new Bundle {
    val in = new Bundle {
      val valid = Input(Bool())
      val predicted_pc = Input(UInt(coreConfig.XLEN.W))
      val write_back = new Bundle {
        val rd = Input(UInt(coreConfig.RegAddrWidth.W))
      }
      val alu = Flipped(new AluInput(coreConfig))
    }
    val out = new Bundle {
      val valid = Output(Bool())
      val write_back = new Bundle {
        val rd = Output(UInt(coreConfig.RegAddrWidth.W))
        val data = Output(UInt(coreConfig.XLEN.W))
      }
    }
  })

  val alu = Module(new Alu(coreConfig))
  alu.io.in <> io.in.alu

  io.out.valid := io.in.valid
  io.out.write_back.rd := io.in.write_back.rd & Fill(
    coreConfig.RegAddrWidth,
    io.out.valid
  )
  io.out.write_back.data := alu.io.out
}
