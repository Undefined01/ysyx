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

class EX(implicit c: CoreConfig) extends Module {
  val io = IO(new Bundle {
    val in_valid = Input(Bool())
    val in = new Bundle {
      val predicted_pc = Input(UInt(c.XLEN.W))
      val commit = Input(new CommitIO)
      val ex = Input(new ExIO)
      val mem = Input(new MemIO)
      val wb = Input(new WriteBackIO)
    }

    val forward = Vec(2, Input(new WriteBackIO))

    val out = new Bundle {
      val prediction_failure = Output(Bool())
      val jump_pc = Output(UInt(c.XLEN.W))
      val commit = Output(new CommitIO)
      val mem = Output(new MemIO)
      val wb = Output(new WriteBackIO)
    }
  })

  def handleForwarding(reg: UInt, data: UInt): UInt = {
    val res = WireDefault(data)
    when(reg =/= 0.U) {
      when(reg === io.forward(0).rd) {
        res := io.forward(0).data
      }.elsewhen(reg === io.forward(1).rd) {
        res := io.forward(1).data
      }
    }
    res
  }

  val alu = Module(new Alu)
  alu.io.in.fn := io.in.ex.fn
  alu.io.in.is_op32 := io.in.ex.is_op32
  val rop1 = handleForwarding(io.in.ex.rs1, io.in.ex.op1)
  val rop2 = handleForwarding(io.in.ex.rs2, io.in.ex.op2)
  alu.io.in.op1 := rop1
  alu.io.in.op2 := Mux(io.in.ex.use_imm, io.in.ex.imm, rop2)
  Debug(
    "EX fn=%d %d=%x %d=%x ; %x %x\n",
    alu.io.in.fn,
    io.in.ex.rs1,
    io.in.ex.op1,
    io.in.ex.rs2,
    io.in.ex.op2,
    alu.io.in.op1,
    alu.io.in.op2
  )

  io.out.mem.en := io.in.mem.en
  io.out.mem.rw := io.in.mem.rw
  io.out.mem.unsigned := io.in.mem.unsigned
  io.out.mem.wWidth := io.in.mem.wWidth
  io.out.mem.addr := alu.io.out
  io.out.mem.wdata := rop2

  io.out.wb.rd := io.in.wb.rd
  io.out.wb.data := alu.io.out

  io.out.commit := io.in.commit
  when(io.in.ex.is_putch) {
    io.out.commit.putch := rop1(7, 0);
  }.otherwise {
    io.out.commit.putch := 0.U;
  }

  io.out.prediction_failure := false.B
  io.out.jump_pc := alu.io.out
  when(io.in_valid && io.in.ex.is_jump) {
    io.out.wb.data := io.in.ex.imm
    when(io.in.predicted_pc =/= io.out.jump_pc) {
      io.out.prediction_failure := true.B
    }
  }
  when(io.in_valid && io.in.ex.is_branch) {
    io.out.jump_pc := Mux(
      alu.io.out(0).asBool,
      io.in.ex.imm,
      io.in.commit.pc + 4.U
    )
    Debug("Alu %d %x; Branch to %x\n", alu.io.in.fn, alu.io.out, io.out.jump_pc)
    when(io.in.predicted_pc =/= io.out.jump_pc) {
      io.out.prediction_failure := true.B
    }
  }
}
