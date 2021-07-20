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

class Alu(coreConfig: CoreConfig) extends Module {
  require(coreConfig.XLEN == 32 || coreConfig.XLEN == 64)
  private val shift_op2_len = coreConfig.XLEN match {
    case 32 => 5; case 64 => 6
  }

  val io = IO(new Bundle {
    val in = new Bundle {
      val fn = Input(UInt(AluFn.bits.W))
      val op32 = Input(Bool())
      val op1 = Input(UInt(coreConfig.XLEN.W))
      val op2 = Input(UInt(coreConfig.XLEN.W))
    }
    val out = Output(UInt(coreConfig.XLEN.W))
  })

  val fn = io.in.fn
  val op32 = io.in.op32
  val op1 = Mux(op32, SignExt(io.in.op1(31, 0), coreConfig.XLEN), io.in.op1)
  val op2 = Mux(op32, SignExt(io.in.op2(31, 0), coreConfig.XLEN), io.in.op2)
  val shift_op2 = Mux(op32, Cat(0.U(1.W), op2(4, 0)), op2(5, 0))

  val add = op1 + op2
  val sub = op1 - op2
  val sll = op1 << shift_op2
  val lt = op1.asSInt < op2.asSInt
  val ltu = op1 < op2
  val slt = ZeroExt(lt.asUInt, coreConfig.XLEN)
  val sltu = ZeroExt(ltu.asUInt, coreConfig.XLEN)
  val xor = op1 ^ op2
  val srl = op1 >> shift_op2
  val sra = (op1.asSInt >> shift_op2).asUInt
  val or = op1 | op2
  val and = op1 & op2

  val ne = xor.orR
  val seq = ZeroExt((!ne).asUInt, coreConfig.XLEN)
  val sne = ZeroExt(ne.asUInt, coreConfig.XLEN)
  val sge = ZeroExt((!lt).asUInt, coreConfig.XLEN)
  val sgeu = ZeroExt((!ltu).asUInt, coreConfig.XLEN)

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
  io.out := Mux(op32, SignExt(res(31, 0), coreConfig.XLEN), res)
}

class EX(coreConfig: CoreConfig) extends Module {
  val io = IO(new Bundle {
    val in_valid = Input(Bool())
    val in = new Bundle {
      val pc = Input(UInt(coreConfig.XLEN.W))
      val predicted_pc = Input(UInt(coreConfig.XLEN.W))
      val ex = new Bundle {
        val fn = Input(UInt(AluFn.bits.W))
        val op32 = Input(Bool())
        val is_jump = Input(Bool())
        val is_branch = Input(Bool())
        val use_imm = Input(Bool())
        val rs1 = Input(UInt(coreConfig.RegAddrWidth.W))
        val rs2 = Input(UInt(coreConfig.RegAddrWidth.W))
        val op1 = Input(UInt(coreConfig.XLEN.W))
        val op2 = Input(UInt(coreConfig.XLEN.W))
        val imm = Input(UInt(coreConfig.XLEN.W))
      }
      val mem = new Bundle {
        val en = Input(Bool())
        val rw = Input(Bool())
        val unsigned = Input(Bool())
        val wWidth = Input(UInt(3.W))
      }
      val write_back = new Bundle {
        val rd = Input(UInt(coreConfig.RegAddrWidth.W))
      }
    }

    val forward = Vec(
      2,
      new Bundle {
        val rd = Input(UInt(coreConfig.RegAddrWidth.W))
        val data = Input(UInt(coreConfig.XLEN.W))
      }
    )

    val out = new Bundle {
      val pc = Output(UInt(coreConfig.XLEN.W))
      val prediction_failure = Output(Bool())
      val jump_pc = Output(UInt(coreConfig.XLEN.W))
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

  val alu = Module(new Alu(coreConfig))
  alu.io.in.fn := io.in.ex.fn
  alu.io.in.op32 := io.in.ex.op32
  val rop1 = handleForwarding(io.in.ex.rs1, io.in.ex.op1)
  val rop2 = handleForwarding(io.in.ex.rs2, io.in.ex.op2)
  alu.io.in.op1 := rop1
  alu.io.in.op2 := Mux(io.in.ex.use_imm, io.in.ex.imm, rop2)
  // Debug(
  //   "EX fn=%d %d=%x %d=%x ; %x %x\n",
  //   alu.io.in.fn,
  //   io.in.ex.rs1,
  //   io.in.ex.op1,
  //   io.in.ex.rs2,
  //   io.in.ex.op2,
  //   alu.io.in.op1,
  //   alu.io.in.op2
  // )

  io.out.mem.en := io.in.mem.en
  io.out.mem.rw := io.in.mem.rw
  io.out.mem.unsigned := io.in.mem.unsigned
  io.out.mem.wWidth := io.in.mem.wWidth
  io.out.mem.addr := alu.io.out
  io.out.mem.wdata := rop2

  io.out.write_back.rd := io.in.write_back.rd
  io.out.write_back.data := alu.io.out

  io.out.pc := io.in.pc
  io.out.prediction_failure := false.B
  io.out.jump_pc := alu.io.out
  when(io.in_valid && io.in.ex.is_jump) {
    io.out.write_back.data := io.in.ex.imm
    when(io.in.predicted_pc =/= io.out.jump_pc) {
      io.out.prediction_failure := true.B
    }
  }
  when(io.in_valid && io.in.ex.is_branch) {
    io.out.jump_pc := Mux(alu.io.out(0).asBool, io.in.ex.imm, io.in.pc + 4.U)
    Debug("Alu %d %x; Branch to %x\n", alu.io.in.fn, alu.io.out, io.out.jump_pc)
    when(io.in.predicted_pc =/= io.out.jump_pc) {
      io.out.prediction_failure := true.B
    }
  }
}
