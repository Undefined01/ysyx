package rvcore.isa

import chisel3._
import chisel3.util._
import rvcore.{CoreConfig, AluFn}
import utils._

object custom extends InstructionSet {
  protected object TrapDecoder extends Decoder {
    def apply(pc: UInt, instr: UInt)(implicit c: CoreConfig): DecodeInfo =
      new DefaultDecodeInfo(pc, instr)
  }

  val instrSet: List[(BitPat, Decoder)] = List(
    // Trap
    (BitPat("b?????????????????????????_1101011"), TrapDecoder)
  )
}

object rv64i_opimm extends InstructionSet {
  protected object OpImmArithDecoder extends Decoder {
    def apply(pc: UInt, instr: UInt)(implicit c: CoreConfig): DecodeInfo =
      new DefaultDecodeInfo(pc, instr) {
        bits.ex.use_imm := true.B
        bits.ex.imm := I_imm
        bits.ex.fn := ZeroExt(funct3, 4)
        bits.wb.rd := rd
      }
  }
  protected object OpImmFunctDecoder extends Decoder {
    def apply(pc: UInt, instr: UInt)(implicit c: CoreConfig): DecodeInfo =
      new DefaultDecodeInfo(pc, instr) {
        bits.ex.use_imm := true.B
        bits.ex.imm := I_imm
        bits.ex.fn := Cat(instr(30), funct3)
        bits.wb.rd := rd
      }
  }

  val instrSet: List[(BitPat, Decoder)] = List(
    // ADDI
    (BitPat("b????????????_?????_000_?????_0010011"), OpImmArithDecoder),
    // SLLI
    (BitPat("b000000??????_?????_001_?????_0010011"), OpImmFunctDecoder),
    // SLTI
    (BitPat("b????????????_?????_010_?????_0010011"), OpImmArithDecoder),
    // SLTIU
    (BitPat("b????????????_?????_011_?????_0010011"), OpImmArithDecoder),
    // XORI
    (BitPat("b????????????_?????_100_?????_0010011"), OpImmArithDecoder),
    // SRLI
    (BitPat("b000000??????_?????_101_?????_0010011"), OpImmFunctDecoder),
    // SRAI
    (BitPat("b010000??????_?????_101_?????_0010011"), OpImmFunctDecoder),
    // ORI
    (BitPat("b????????????_?????_110_?????_0010011"), OpImmArithDecoder),
    // ANDI
    (BitPat("b????????????_?????_111_?????_0010011"), OpImmArithDecoder)
  )
}

object rv64i_op extends InstructionSet {
  protected object OpFunctDecoder extends Decoder {
    def apply(pc: UInt, instr: UInt)(implicit c: CoreConfig): DecodeInfo =
      new DefaultDecodeInfo(pc, instr) {
        bits.ex.fn := Cat(instr(30), funct3)
        bits.wb.rd := rd
      }
  }

  val instrSet: List[(BitPat, Decoder)] = List(
    // ADD
    (BitPat("b0000000_?????_?????_000_?????_0110011"), OpFunctDecoder),
    // SUB
    (BitPat("b0100000_?????_?????_000_?????_0110011"), OpFunctDecoder),
    // SLL
    (BitPat("b0000000_?????_?????_001_?????_0110011"), OpFunctDecoder),
    // SLT
    (BitPat("b0000000_?????_?????_010_?????_0110011"), OpFunctDecoder),
    // SLTI
    (BitPat("b0000000_?????_?????_011_?????_0110011"), OpFunctDecoder),
    // XOR
    (BitPat("b0000000_?????_?????_100_?????_0110011"), OpFunctDecoder),
    // SRL
    (BitPat("b0000000_?????_?????_101_?????_0110011"), OpFunctDecoder),
    // SRA
    (BitPat("b0100000_?????_?????_101_?????_0110011"), OpFunctDecoder),
    // OR
    (BitPat("b0000000_?????_?????_110_?????_0110011"), OpFunctDecoder),
    // AND
    (BitPat("b0000000_?????_?????_111_?????_0110011"), OpFunctDecoder)
  )
}

object rv64i_u extends InstructionSet {
  protected object LuiDecoder extends Decoder {
    def apply(pc: UInt, instr: UInt)(implicit c: CoreConfig): DecodeInfo =
      new DefaultDecodeInfo(pc, instr) {
        bits.use_op1 := true.B
        bits.ex.rs1 := 0.U
        bits.ex.op1 := 0.U
        bits.ex.use_imm := true.B
        bits.ex.imm := U_imm
        bits.ex.fn := AluFn.ADD.U
        bits.wb.rd := rd
      }
  }
  protected object AuiPcDecoder extends Decoder {
    def apply(pc: UInt, instr: UInt)(implicit c: CoreConfig): DecodeInfo =
      new DefaultDecodeInfo(pc, instr) {
        bits.use_op1 := true.B
        bits.ex.rs1 := 0.U
        bits.ex.op1 := pc
        bits.ex.use_imm := true.B
        bits.ex.imm := U_imm
        bits.ex.fn := AluFn.ADD.U
        bits.wb.rd := rd
      }
  }

  val instrSet: List[(BitPat, Decoder)] = List(
    // LUI
    (BitPat("b????????????????????_?????_0110111"), LuiDecoder),
    // AUIPC
    (BitPat("b????????????????????_?????_0010111"), AuiPcDecoder)
  )
}

object rv64i extends InstructionSet {
  val instrSet = List(rv64i_opimm, rv64i_op, rv64i_u, custom).map(_.instrSet).flatten
}
