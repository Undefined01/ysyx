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

  val pc = Wire(UInt(coreConfig.XLEN.W))
  pc := coreConfig.InitialPC.U

  val regs = Module(new RegFile(coreConfig))
  val ifu = Module(new IF(coreConfig))
  val idu = Module(new ID(coreConfig))
  val id_ex = Module(new ID_EX(coreConfig))
  val exu = Module(new EX(coreConfig))
  val ex_mem = Module(new EX_MEM(coreConfig))
  val memu = Module(new MEM(coreConfig))
  val wbu = Module(new WB(coreConfig))

  Debug("-----------------------------------\n")
  ifu.io.in.pc := pc
  ifu.io.if_io <> mem.io.rport
  ifu.io.out_ready := id_ex.io.in_ready
  Debug(
    ifu.io.if_io.en,
    "IF: fetch pc=0x%x 0x%x\n",
    ifu.io.if_io.addr,
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
    "ID out: predicted_pc=0x%x fn=%d op1=%x op2=%x mem=%d%d wb=%d\n",
    idu.io.out.predicted_pc,
    idu.io.out.alu.fn,
    idu.io.out.alu.op1,
    idu.io.out.alu.op2,
    idu.io.out.mem.en,
    idu.io.out.mem.rw,
    idu.io.out.write_back.rd
  )

  id_ex.io.in <> idu.io.out
  id_ex.io.out_ready := ex_mem.io.in_ready

  exu.io.in.valid := id_ex.io.out.valid
  exu.io.in.predicted_pc := id_ex.io.out.predicted_pc
  exu.io.in.alu := id_ex.io.out.alu
  exu.io.in.mem := id_ex.io.out.mem
  exu.io.in.write_back := id_ex.io.out.write_back
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
    "EX out: mem=%d%d %x; write %d=0x%x\n",
    exu.io.out.mem.en,
    exu.io.out.mem.rw,
    exu.io.out.mem.addr,
    exu.io.out.write_back.rd,
    exu.io.out.write_back.data
  )

  ex_mem.io.in := exu.io.out
  ex_mem.io.out_ready := memu.io.in_ready

  memu.io.in.valid := ex_mem.io.out.valid
  memu.io.in.mem := ex_mem.io.out.mem
  memu.io.in.write_back := ex_mem.io.out.write_back
  memu.io.out.mem <> mem.io.rwport

  wbu.io.in.valid := memu.io.out.valid
  wbu.io.in.write_back := memu.io.out.write_back
  wbu.io.reg_io <> regs.io.wport
  idu.io.forward1 <> memu.io.out.write_back
  Debug(
    wbu.io.in.valid && wbu.io.in.write_back.rd =/= 0.U,
    "WB in: reg%d=%x\n",
    wbu.io.in.write_back.rd,
    wbu.io.in.write_back.data,
  )

  if (coreConfig.DebugPin) {
    io.debug.get.reg := regs.io.debug.get.reg
    io.debug.get.if_pc := ifu.io.out.pc
    io.debug.get.if_instr := ifu.io.out.instr
  }
}
