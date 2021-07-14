package core

import chisel3._
import chisel3.util._
import utils._

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
  val SEQ = 1 + 8
  val SNE = 4 + 8
  val SGE = 2 + 8
  val SGEU = 3 + 8

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
      AluFn.ADD.U -> add_sub,
      AluFn.SUB.U -> add_sub,
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
}

class EX(coreConfig: CoreConfig) extends Module {
  val io = IO(new Bundle {
    val in = new Bundle {
      val valid = Input(Bool())
      val predicted_pc = Input(UInt(coreConfig.XLEN.W))
      val mem = new Bundle {
        val en = Input(Bool())
        val rw = Input(Bool())
        val unsigned = Input(Bool())
        val wWidth = Input(UInt(3.W))
        val wdata = Input(UInt(coreConfig.XLEN.W))
      }
      val write_back = new Bundle {
        val rd = Input(UInt(coreConfig.RegAddrWidth.W))
      }
      val alu = Flipped(new AluInput(coreConfig))
    }
    val out = new Bundle {
      val valid = Output(Bool())
      val mem = new Bundle {
        val en = Output(Bool())
        val rw = Output(Bool())
        val unsigned = Output(Bool())
        val wWidth = Output(UInt(3.W))
        val addr = Output(UInt(coreConfig.XLEN.W))
        val wdata = Output(UInt(coreConfig.XLEN.W))
      }
      val write_back = new Bundle {
        val rd = Output(UInt(coreConfig.RegAddrWidth.W))
        val data = Output(UInt(coreConfig.XLEN.W))
      }
    }
  })

  val alu = Module(new Alu(coreConfig))
  alu.io.in <> io.in.alu

  io.out.valid := io.in.valid

  io.out.mem.en := io.in.mem.en
  io.out.mem.rw := io.in.mem.rw
  io.out.mem.unsigned := io.in.mem.unsigned
  io.out.mem.wWidth := io.in.mem.wWidth
  io.out.mem.wdata := io.in.mem.wdata
  io.out.mem.addr := alu.io.out
  
  io.out.write_back.rd := io.in.write_back.rd
  io.out.write_back.data := alu.io.out
}
