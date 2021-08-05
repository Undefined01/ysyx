package rvcore.isa

import chisel3._
import chisel3.util._
import rvcore.CoreConfig
import utils._

object rv64i extends InstructionSet {
  protected object OpImmArithDecoder extends Decoder {
    def apply(pc: UInt, instr: UInt): DecodeInfo =
      new DefaultDecodeInfo(pc, instr) {
        override val use_imm = true.B
        override val imm = I_imm
        override val alufn = ZeroExt(funct3, 4)
        override val wb_rd = rd
      }
  }

  val instrSet: Array[(BitPat, Decoder)] = Array(
    (BitPat("b????????????_?????_000_?????_0010011"), OpImmArithDecoder)
  )
}
