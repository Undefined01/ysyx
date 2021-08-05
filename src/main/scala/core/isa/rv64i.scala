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
  protected object PutchDecoder extends Decoder {
    def apply(pc: UInt, instr: UInt)(implicit c: CoreConfig): DecodeInfo =
      new DefaultDecodeInfo(pc, instr) {
        bits.commit.is_putch := true.B
      }
  }

  val instrSet: List[(BitPat, Decoder)] = List(
    // Trap
    (BitPat("b?????????????????????????_1101011"), TrapDecoder),
    // Putch
    (BitPat("b?????????????????????????_1111011"), PutchDecoder)
  )
}

protected case class ArithDecoderHelper(use_funct6: Boolean) extends Decoder {
  def apply(pc: UInt, instr: UInt)(implicit c: CoreConfig): DecodeInfo =
    new DefaultDecodeInfo(pc, instr) {
      bits.ex.is_op32 := instr(3)
      bits.ex.use_imm := !instr(5)
      bits.ex.imm := I_imm
      bits.ex.fn := Cat(instr(30) & use_funct6.B, funct3)
      bits.wb.rd := rd
    }
}
protected object ArithDecoder extends ArithDecoderHelper(true) {}
protected object ArithImmDecoder extends ArithDecoderHelper(false) {}

object rv64i_opimm extends InstructionSet {
  val instrSet: List[(BitPat, Decoder)] = List(
    // ADDI
    (BitPat("b????????????_?????_000_?????_0010011"), ArithImmDecoder),
    // SLLI
    (BitPat("b000000_??????_?????_001_?????_0010011"), ArithDecoder),
    // SLTI
    (BitPat("b????????????_?????_010_?????_0010011"), ArithImmDecoder),
    // SLTIU
    (BitPat("b????????????_?????_011_?????_0010011"), ArithImmDecoder),
    // XORI
    (BitPat("b????????????_?????_100_?????_0010011"), ArithImmDecoder),
    // SRLI
    (BitPat("b000000_??????_?????_101_?????_0010011"), ArithDecoder),
    // SRAI
    (BitPat("b010000_??????_?????_101_?????_0010011"), ArithDecoder),
    // ORI
    (BitPat("b????????????_?????_110_?????_0010011"), ArithImmDecoder),
    // ANDI
    (BitPat("b????????????_?????_111_?????_0010011"), ArithImmDecoder)
  )
}

object rv64i_op extends InstructionSet {
  val instrSet: List[(BitPat, Decoder)] = List(
    // ADD
    (BitPat("b0000000_?????_?????_000_?????_0110011"), ArithDecoder),
    // SUB
    (BitPat("b0100000_?????_?????_000_?????_0110011"), ArithDecoder),
    // SLL
    (BitPat("b0000000_?????_?????_001_?????_0110011"), ArithDecoder),
    // SLT
    (BitPat("b0000000_?????_?????_010_?????_0110011"), ArithDecoder),
    // SLTI
    (BitPat("b0000000_?????_?????_011_?????_0110011"), ArithDecoder),
    // XOR
    (BitPat("b0000000_?????_?????_100_?????_0110011"), ArithDecoder),
    // SRL
    (BitPat("b0000000_?????_?????_101_?????_0110011"), ArithDecoder),
    // SRA
    (BitPat("b0100000_?????_?????_101_?????_0110011"), ArithDecoder),
    // OR
    (BitPat("b0000000_?????_?????_110_?????_0110011"), ArithDecoder),
    // AND
    (BitPat("b0000000_?????_?????_111_?????_0110011"), ArithDecoder)
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

object rv64i_ldst extends InstructionSet {
  protected object LoadDecoder extends Decoder {
    def apply(pc: UInt, instr: UInt)(implicit c: CoreConfig): DecodeInfo =
      new DefaultDecodeInfo(pc, instr) {
        bits.ex.use_imm := true.B
        bits.ex.imm := I_imm
        bits.ex.fn := AluFn.ADD.U
        bits.wb.rd := rd
        bits.mem.en := true.B
        bits.mem.rw := false.B
        bits.mem.unsigned := funct3(2).asBool
        bits.mem.wWidth := funct3(1, 0)
      }
  }
  protected object StoreDecoder extends Decoder {
    def apply(pc: UInt, instr: UInt)(implicit c: CoreConfig): DecodeInfo =
      new DefaultDecodeInfo(pc, instr) {
        bits.ex.use_imm := true.B
        bits.ex.imm := S_imm
        bits.ex.fn := AluFn.ADD.U
        bits.mem.en := true.B
        bits.mem.rw := true.B
        bits.mem.wWidth := funct3(1, 0)
      }
  }

  val instrSet: List[(BitPat, Decoder)] = List(
    // LB
    (BitPat("b????????????_?????_000_?????_0000011"), LoadDecoder),
    // LH
    (BitPat("b????????????_?????_001_?????_0000011"), LoadDecoder),
    // LW
    (BitPat("b????????????_?????_010_?????_0000011"), LoadDecoder),
    // LD
    (BitPat("b????????????_?????_011_?????_0000011"), LoadDecoder),
    // LBU
    (BitPat("b????????????_?????_100_?????_0000011"), LoadDecoder),
    // LHU
    (BitPat("b????????????_?????_101_?????_0000011"), LoadDecoder),
    // LWU
    (BitPat("b????????????_?????_110_?????_0000011"), LoadDecoder),
    // SB
    (BitPat("b???????_?????_?????_000_?????_0100011"), StoreDecoder),
    // SH
    (BitPat("b???????_?????_?????_001_?????_0100011"), StoreDecoder),
    // SW
    (BitPat("b???????_?????_?????_010_?????_0100011"), StoreDecoder),
    // SD
    (BitPat("b???????_?????_?????_011_?????_0100011"), StoreDecoder)
  )
}

object rv64i_jump extends InstructionSet {
  protected object JalDecoder extends Decoder {
    def apply(pc: UInt, instr: UInt)(implicit c: CoreConfig): DecodeInfo =
      new DefaultDecodeInfo(pc, instr) {
        bits.ex.is_jump := true.B

        bits.use_op1 := true.B
        bits.ex.rs1 := 0.U
        bits.ex.op1 := pc
        bits.use_op2 := true.B
        bits.ex.rs2 := 0.U
        bits.ex.op2 := J_imm
        bits.ex.fn := AluFn.ADD.U
        bits.wb.rd := rd

        bits.ex.use_imm := false.B
        bits.ex.imm := pc + 4.U
      }
  }
  protected object JalrDecoder extends Decoder {
    def apply(pc: UInt, instr: UInt)(implicit c: CoreConfig): DecodeInfo =
      new DefaultDecodeInfo(pc, instr) {
        bits.ex.is_jump := true.B

        bits.use_op2 := true.B
        bits.ex.rs2 := 0.U
        bits.ex.op2 := I_imm
        bits.ex.fn := AluFn.ADD.U
        bits.wb.rd := rd

        bits.ex.use_imm := false.B
        bits.ex.imm := pc + 4.U
      }
  }

  val instrSet: List[(BitPat, Decoder)] = List(
    // JAL
    (BitPat("b????????????????????_?????_1101111"), JalDecoder),
    // JALR
    (BitPat("b????????????_?????_000_?????_1100111"), JalrDecoder)
  )
}

object rv64i_branch extends InstructionSet {
  protected case class BranchDecoder(funct: Int) extends Decoder {
    def apply(pc: UInt, instr: UInt)(implicit c: CoreConfig): DecodeInfo =
      new DefaultDecodeInfo(pc, instr) {
        bits.ex.is_branch := true.B

        bits.ex.fn := funct.U

        bits.ex.use_imm := false.B
        bits.ex.imm := pc + B_imm
      }
  }

  val instrSet: List[(BitPat, Decoder)] = List(
    // BEQ
    (
      BitPat("b???????_?????_?????_000_?????_1100011"),
      BranchDecoder(AluFn.SEQ)
    ),
    // BNE
    (
      BitPat("b???????_?????_?????_001_?????_1100011"),
      BranchDecoder(AluFn.SNE)
    ),
    // BLT
    (
      BitPat("b???????_?????_?????_100_?????_1100011"),
      BranchDecoder(AluFn.SLT)
    ),
    // BGE
    (
      BitPat("b???????_?????_?????_101_?????_1100011"),
      BranchDecoder(AluFn.SGE)
    ),
    // BLTU
    (
      BitPat("b???????_?????_?????_110_?????_1100011"),
      BranchDecoder(AluFn.SLTU)
    ),
    // BGEU
    (
      BitPat("b???????_?????_?????_111_?????_1100011"),
      BranchDecoder(AluFn.SGEU)
    )
  )
}

object rv64i_op32imm extends InstructionSet {
  val instrSet: List[(BitPat, Decoder)] = List(
    // ADDIW
    (BitPat("b????????????_?????_000_?????_0011011"), ArithImmDecoder),
    // SLLIW
    (BitPat("b000000_??????_?????_001_?????_0011011"), ArithDecoder),
    // SRLIW
    (BitPat("b000000_??????_?????_101_?????_0011011"), ArithDecoder),
    // SRAIW
    (BitPat("b010000_??????_?????_101_?????_0011011"), ArithDecoder)
  )
}

object rv64i_op32 extends InstructionSet {
  val instrSet: List[(BitPat, Decoder)] = List(
    // ADDW
    (BitPat("b0000000_?????_?????_000_?????_0111011"), ArithDecoder),
    // SUBW
    (BitPat("b0100000_?????_?????_000_?????_0111011"), ArithDecoder),
    // SLLW
    (BitPat("b0000000_?????_?????_001_?????_0111011"), ArithDecoder),
    // SRLW
    (BitPat("b0000000_?????_?????_101_?????_0111011"), ArithDecoder),
    // SRAW
    (BitPat("b0100000_?????_?????_101_?????_0111011"), ArithDecoder)
  )
}

object rv64i extends InstructionSet {
  val instrSet =
    List(
      rv64i_opimm,
      rv64i_op,
      rv64i_u,
      rv64i_ldst,
      rv64i_jump,
      rv64i_branch,
      rv64i_op32imm,
      rv64i_op32,
      custom
    )
      .map(_.instrSet)
      .flatten
}
