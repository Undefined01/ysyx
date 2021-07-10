package core

import chisel3._
import chisel3.util._
import utils.Logger.Debug

class Core(coreConfig: CoreConfig) extends Module {
  val io = IO(new Bundle {})

  val mem = Module(
    new Memory(
      addrWidth = coreConfig.XLEN,
      dataBytes = coreConfig.XLEN / 8,
      depth = coreConfig.MemorySize / coreConfig.XLEN,
      memoryFile = coreConfig.MemoryFile
    )
  )
  mem.io.rwport.en := false.B
  mem.io.rwport.rw := false.B
  mem.io.rwport.addr := DontCare
  mem.io.rwport.wmask := DontCare
  mem.io.rwport.wdata := DontCare

  val pc = RegInit(coreConfig.InitialPC.U)
  val regs = Module(new RegisterFile(coreConfig))

  Debug("-----------------------------------\n")
  val ifu = Module(new IF(coreConfig))
  ifu.io.in.pc := pc
  ifu.io.if_io <> mem.io.rport

  val idu = Module(new ID(coreConfig))
  idu.io.in.valid := ifu.io.out.valid
  idu.io.in.pc := ifu.io.out.pc
  idu.io.in.instr := ifu.io.out.instr
  idu.io.reg_io <> regs.io.rport
  when(idu.io.out.valid) {
    pc := idu.io.out.predicted_pc
  }
  Debug(idu.io.in.valid, "ID in: pc=0x%x 0x%x\n", idu.io.in.pc, idu.io.in.instr)
  Debug(idu.io.in.valid, "ID out: predicted=0x%x\n", idu.io.out.predicted_pc)

  val exu = Module(new EX(coreConfig))
  exu.io.in.valid := idu.io.out.valid
  exu.io.in.predicted_pc := idu.io.out.predicted_pc
  exu.io.in.alu := idu.io.out.alu
  exu.io.in.write_back := idu.io.out.write_back
  Debug(exu.io.in.valid, "EX in: pc=0x%x\n", exu.io.in.predicted_pc)
  Debug(exu.io.in.valid, "EX out: write %d=0x%x\n", exu.io.out.write_back.rd, exu.io.out.write_back.data)

  val wbu = Module(new WB(coreConfig))
  wbu.io.in.valid := exu.io.out.valid
  wbu.io.in.rd := exu.io.out.write_back.rd
  wbu.io.in.data := exu.io.out.write_back.data
  wbu.io.reg_io <> regs.io.wport

  pc := pc + 4.U

}
