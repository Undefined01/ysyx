package core

import chisel3._
import chisel3.util._

class Core(implicit coreConfig: CoreConfig) extends Module {
  val io = IO(new Bundle {})

  val mem = Module(
    new Memory(
      addrWidth = coreConfig.XLEN,
      dataBytes = coreConfig.XLEN / 8,
      depth = coreConfig.MemorySize / coreConfig.XLEN,
      memoryFile = coreConfig.MemoryFile
    )
  )

  val pc = RegInit(coreConfig.InitialPC.U)
  val regs = Module(new RegisterFile(coreConfig))

  val ifu = Module(new IF(coreConfig))
  ifu.io.in.pc := pc
  ifu.io.if_io <> mem.io.rport

  val idu = Module(new ID(coreConfig))
  idu.io.in.valid := ifu.io.out.valid
  idu.io.in.pc := ifu.io.out.pc
  idu.io.in.instr := ifu.io.out.instr
  when(idu.io.out.valid) {
    pc := idu.io.out.predicted_pc
  }

  val exu = Module(new EX(coreConfig))
  exu.io.in.valid := idu.out.valid
  exu.io.in.alu := idu.out.alu

  val wbu = Module(new WB(coreConfig))
  wbu.io.in.valid := exu.io.out.valid
  wbu.io.in.rd := exu.io.out.write_back.rd
  wbu.io.in.data := exu.io.out.write_back.data

  pc := pc + 4.U

}
