package core

import chisel3._
import chisel3.util._
import utils._

object DecodeConstant {
  val OpImm = BigInt("0010011", 2)

}

class Decoder(coreConfig: CoreConfig) extends Module {
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

    val out = new Bundle {
      val valid = Output(Bool())
      val predicted_pc = Output(UInt(coreConfig.XLEN.W))
      val alu = Flipped(new AluInput(coreConfig))
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
  io.out.valid := io.in.valid
  io.out.alu := DontCare
  io.out.write_back.rd := 0.U

  io.out.predicted_pc := io.in.pc + 4.U

  switch(opcode) {
    is(DecodeConstant.OpImm.U) {
      io.out.alu.fn := ZeroExt(funct3, AluFn.bits)
      io.out.alu.op1 := io.reg_io.rdata(0)
      io.out.alu.op2 := I_imm
      io.out.write_back.rd := rd
    }
  }
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

    val out = new Bundle {
      val valid = Output(Bool())
      val predicted_pc = Output(UInt(coreConfig.XLEN.W))
      val alu = Flipped(new AluInput(coreConfig))
      val write_back = new Bundle {
        val rd = Output(UInt(coreConfig.RegAddrWidth.W))
      }
    }
  })

  val decoder = Module(new Decoder(coreConfig))
  decoder.io.in <> io.in
  decoder.io.reg_io <> io.reg_io
  val out = RegNext(decoder.io.out)
  io.out <> out
}
