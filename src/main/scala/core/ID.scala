package core

import chisel3._
import chisel3.util._
import utils._

object DecodeConstant {
  val OpImm = BigInt("0010011", 2)
  val Op = BigInt("0110011", 2)
  val Load = BigInt("0000011", 2)
  val Store = BigInt("0100011", 2)
}

class ID(coreConfig: CoreConfig) extends Module {
  val io = IO(new Bundle {
    val in = new Bundle {
      val valid = Input(Bool())
      val pc = Input(UInt(coreConfig.XLEN.W))
      val instr = Input(UInt(coreConfig.InstrLen.W))
    }

    val reg_io = new Bundle {
      val raddr = Output(Vec(2, UInt(coreConfig.RegAddrWidth.W)))
      val rdata = Input(Vec(2, UInt(coreConfig.XLEN.W)))
    }

    val forward0 =
      new Bundle {
        val rd = Input(UInt(coreConfig.RegAddrWidth.W))
        val data = Input(UInt(coreConfig.XLEN.W))
      }
    val forward1 =
      new Bundle {
        val rd = Input(UInt(coreConfig.RegAddrWidth.W))
        val data = Input(UInt(coreConfig.XLEN.W))
      }

    val out = new Bundle {
      val valid = Output(Bool())
      val predicted_pc = Output(UInt(coreConfig.XLEN.W))
      val alu = new AluInput(coreConfig)
      val mem = new Bundle {
        val en = Output(Bool())
        val rw = Output(Bool())
        val unsigned = Output(Bool())
        val wWidth = Output(UInt(3.W))
        val wdata = Output(UInt(coreConfig.XLEN.W))
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

  io.reg_io.raddr(0) := rs1
  io.reg_io.raddr(1) := rs2
  val rop1 = Wire(UInt(coreConfig.XLEN.W))
  rop1 := io.reg_io.rdata(0)
  when(rs1 =/= 0.U) {
    when(rs1 === io.forward0.rd) {
      rop1 := io.forward0.data
    }.elsewhen(rs1 === io.forward1.rd) {
      rop1 := io.forward1.data
    }
  }
  val rop2 = Wire(UInt(coreConfig.XLEN.W))
  rop2 := io.reg_io.rdata(1)
  when(rs2 =/= 0.U) {
    when(rs2 === io.forward0.rd) {
      rop2 := io.forward0.data
    }.elsewhen(rs2 === io.forward1.rd) {
      rop2 := io.forward1.data
    }
  }

  io.out.valid := io.in.valid
  io.out.alu := DontCare
  io.out.mem.en := false.B
  io.out.mem.rw := DontCare
  io.out.mem.unsigned := funct3(2).asBool
  io.out.mem.wWidth := funct3(1, 0)
  io.out.mem.wdata := rop2
  io.out.write_back.rd := 0.U

  io.out.predicted_pc := io.in.pc + 4.U

  switch(opcode) {
    is(DecodeConstant.OpImm.U) {
      io.out.alu.fn := Cat((funct3 === 5.U && funct7 =/= 0.U).asUInt, funct3)
      io.out.alu.op1 := rop1
      io.out.alu.op2 := I_imm
      io.out.write_back.rd := rd
    }
    is(DecodeConstant.Op.U) {
      io.out.alu.fn := Cat((funct7 =/= 0.U).asUInt, funct3)
      io.out.alu.op1 := rop1
      io.out.alu.op2 := rop2
      io.out.write_back.rd := rd
    }
    is(DecodeConstant.Store.U) {
      io.out.alu.fn := AluFn.ADD.U
      io.out.alu.op1 := rop1
      io.out.alu.op2 := S_imm
      io.out.mem.en := true.B
      io.out.mem.rw := true.B
    }
  }
}