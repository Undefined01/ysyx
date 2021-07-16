package core

import chisel3._
import chisel3.util._
import utils._

object DecodeConstant {
  val OpImm = BigInt("0010011", 2)
  val Op = BigInt("0110011", 2)
  val Load = BigInt("0000011", 2)
  val Store = BigInt("0100011", 2)
  val Jal = BigInt("1101111", 2)
  val Jalr = BigInt("1100111", 2)
  val Branch = BigInt("1100011", 2)
}

class ID(coreConfig: CoreConfig) extends Module {
  val io = IO(new Bundle {
    val in = new Bundle {
      val pc = Input(UInt(coreConfig.XLEN.W))
      val instr = Input(UInt(coreConfig.InstrLen.W))
    }

    val reg_io = new Bundle {
      val raddr = Output(Vec(2, UInt(coreConfig.RegAddrWidth.W)))
      val rdata = Input(Vec(2, UInt(coreConfig.XLEN.W)))
    }

    val out = new Bundle {
      val pc = Output(UInt(coreConfig.XLEN.W))
      val predicted_pc = Output(UInt(coreConfig.XLEN.W))
      val ex = new Bundle {
        val fn = Output(UInt(AluFn.bits.W))
        val is_jump = Output(Bool())
        val is_branch = Output(Bool())
        val use_imm = Output(Bool())
        val rs1 = Output(UInt(coreConfig.RegAddrWidth.W))
        val rs2 = Output(UInt(coreConfig.RegAddrWidth.W))
        val op1 = Output(UInt(coreConfig.XLEN.W))
        val op2 = Output(UInt(coreConfig.XLEN.W))
        val imm = Output(UInt(coreConfig.XLEN.W))
      }
      val mem = new Bundle {
        val en = Output(Bool())
        val rw = Output(Bool())
        val unsigned = Output(Bool())
        val wWidth = Output(UInt(3.W))
      }
      val write_back = new Bundle {
        val rd = Output(UInt(coreConfig.RegAddrWidth.W))
      }
    }
  })

  val instr = io.in.instr

  val opcode = instr(6, 0)
  val funct3 = instr(14, 12)
  val funct7 = instr(31, 25)

  val rd = instr(11, 7)
  val rs1 = instr(19, 15)
  val rs2 = instr(24, 20)

  val I_imm = SignExt(instr(31, 20), coreConfig.XLEN)
  val S_imm = SignExt(Cat(instr(31, 25), instr(11, 7)), coreConfig.XLEN)
  val B_imm = SignExt(
    Cat(instr(31), instr(7), instr(30, 25), instr(11, 8), 0.U(1.W)),
    coreConfig.XLEN
  )
  val U_imm = SignExt(Cat(instr(31, 12), 0.U(12.W)), coreConfig.XLEN)
  val J_imm = SignExt(
    Cat(instr(31), instr(19, 12), instr(20), instr(30, 21), 0.U(1.W)),
    coreConfig.XLEN
  )

  def I_type() = {
    io.out.ex.use_imm := true.B
    io.out.ex.imm := I_imm
    io.out.write_back.rd := rd
  }
  def R_type() = {
    io.out.ex.use_imm := false.B
    io.out.write_back.rd := rd
  }
  def S_type() = {
    io.out.ex.use_imm := true.B
    io.out.ex.imm := S_imm
  }
  def J_type() = {
    io.out.ex.use_imm := false.B
    io.out.ex.rs2 := 0.U
    io.out.ex.op2 := J_imm
    io.out.ex.imm := io.in.pc + 4.U
    io.out.write_back.rd := rd
  }
  def B_type() = {
    io.out.ex.use_imm := false.B
    io.out.ex.imm := io.in.pc + B_imm
  }

  io.reg_io.raddr(0) := rs1
  io.reg_io.raddr(1) := rs2
  io.out.pc := io.in.pc

  io.out.ex := DontCare
  io.out.ex.rs1 := rs1
  io.out.ex.rs2 := rs2
  io.out.ex.op1 := io.reg_io.rdata(0)
  io.out.ex.op2 := io.reg_io.rdata(1)
  io.out.ex.is_jump := false.B
  io.out.ex.is_branch := false.B

  io.out.mem.en := false.B
  io.out.mem.rw := DontCare
  io.out.mem.unsigned := funct3(2).asBool
  io.out.mem.wWidth := funct3(1, 0)
  io.out.write_back.rd := 0.U

  io.out.predicted_pc := io.in.pc + 4.U

  switch(opcode) {
    is(DecodeConstant.OpImm.U) {
      I_type()
      io.out.ex.fn := Cat((funct3 === 5.U && funct7 =/= 0.U).asUInt, funct3)
    }
    is(DecodeConstant.Op.U) {
      R_type()
      io.out.ex.fn := Cat((funct7 =/= 0.U).asUInt, funct3)
    }
    is(DecodeConstant.Load.U) {
      I_type()
      io.out.ex.fn := AluFn.ADD.U
      io.out.mem.en := true.B
      io.out.mem.rw := false.B
    }
    is(DecodeConstant.Store.U) {
      S_type()
      io.out.ex.fn := AluFn.ADD.U
      io.out.mem.en := true.B
      io.out.mem.rw := true.B
    }
    is(DecodeConstant.Jal.U) {
      J_type()
      io.out.ex.rs1 := 0.U
      io.out.ex.op1 := io.in.pc
      io.out.ex.fn := AluFn.ADD.U
      io.out.ex.is_jump := true.B
    }
    is(DecodeConstant.Jalr.U) {
      J_type()
      io.out.ex.fn := AluFn.ADD.U
      io.out.ex.is_jump := true.B
    }
    is(DecodeConstant.Branch.U) {
      B_type()
      io.out.ex.fn := MuxLookup(
        funct3,
        DontCare,
        Array(
          0.U -> AluFn.SEQ.U,
          1.U -> AluFn.SNE.U,
          4.U -> AluFn.SLT.U,
          5.U -> AluFn.SGE.U,
          6.U -> AluFn.SLTU.U,
          7.U -> AluFn.SGEU.U
        )
      )
      io.out.ex.is_branch := true.B
    }
  }
}
