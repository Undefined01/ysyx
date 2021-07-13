package core

import chisel3._
import chisel3.util._
import utils.Logger.Debug

class Core(coreConfig: CoreConfig) extends Module {
  val io = IO(new Bundle {
    val debug =
      if (coreConfig.DebugPin) Some(new Bundle {
        val reg = Output(Vec(32, UInt(coreConfig.XLEN.W)))
        val if_pc = Output(UInt(coreConfig.XLEN.W))
        val if_instr = Output(UInt(coreConfig.InstrLen.W))
      })
      else None
  })

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

  val pc = Wire(UInt(coreConfig.XLEN.W))
  pc := coreConfig.InitialPC.U

  val regs = Module(new RegFile(coreConfig))
  val ifu = Module(new IF(coreConfig))
  val idu = Module(new ID(coreConfig))
  val exu = Module(new EX(coreConfig))
  val wbu = Module(new WB(coreConfig))

  Debug("-----------------------------------\n")
  ifu.io.in.pc := pc
  ifu.io.if_io <> mem.io.rport
  Debug(
    ifu.io.if_io.en,
    "IF: fetch pc=0x%x 0x%x\n",
    ifu.io.in.pc,
    ifu.io.if_io.data.foldLeft(0.U(1.W))(Cat(_, _))
  )

  idu.io.in.valid := ifu.io.out.valid
  idu.io.in.pc := ifu.io.out.pc
  idu.io.in.instr := ifu.io.out.instr
  idu.io.reg_io <> regs.io.rport
  when(idu.io.out.valid) {
    pc := idu.io.out.predicted_pc
  }
  Debug(idu.io.in.valid, "ID in: pc=0x%x 0x%x\n", idu.io.in.pc, idu.io.in.instr)
  Debug(
    idu.io.out.valid,
    "ID out: predicted_pc=0x%x fn=%d op1=%x op2=%x\n",
    idu.io.out.predicted_pc,
    idu.io.out.alu.fn,
    idu.io.out.alu.op1,
    idu.io.out.alu.op2
  )

  exu.io.in.valid := RegNext(idu.io.out.valid, false.B)
  exu.io.in.predicted_pc := RegNext(idu.io.out.predicted_pc)
  exu.io.in.alu := RegNext(idu.io.out.alu)
  exu.io.in.write_back := RegNext(idu.io.out.write_back)
  idu.io.forward0 <> exu.io.out.write_back
  Debug(
    exu.io.in.valid,
    "EX in: predicted_pc=0x%x fn=%d op1=%x op2=%x\n",
    exu.io.in.predicted_pc,
    exu.io.in.alu.fn,
    exu.io.in.alu.op1,
    exu.io.in.alu.op2
  )
  Debug(
    exu.io.in.valid,
    "EX out: write %d=0x%x\n",
    exu.io.out.write_back.rd,
    exu.io.out.write_back.data
  )

  wbu.io.in.valid := RegNext(exu.io.out.valid, false.B)
  val wbu_write_back = RegNext(exu.io.out.write_back)
  wbu.io.in.write_back := wbu_write_back
  wbu.io.reg_io <> regs.io.wport
  idu.io.forward1 <> wbu_write_back

  if (coreConfig.DebugPin) {
    io.debug.get.reg := regs.io.debug.get.reg
    io.debug.get.if_pc := ifu.io.out.pc
    io.debug.get.if_instr := ifu.io.out.instr
  }
}
