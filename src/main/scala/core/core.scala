package rvcore

import chisel3._
import chisel3.util._
import utils.Logger.Debug

import device.RAM.RamIo

class RvCore(coreConfig: CoreConfig) extends Module {
  val io = IO(new Bundle {
    val ram = new RamIo(coreConfig.XLEN, coreConfig.XLEN / 8)
    val debug =
      if (coreConfig.DebugPin) Some(new Bundle {
        val reg = Output(Vec(32, UInt(coreConfig.XLEN.W)))
        val if_pc = Output(UInt(coreConfig.XLEN.W))
        val if_instr = Output(UInt(coreConfig.InstrLen.W))
      })
      else None
  })

  val pc = Wire(UInt(coreConfig.XLEN.W))

  val regs = Module(new RegFile(coreConfig))

  val ifu = Module(new IF(coreConfig))
  val idu = Module(new ID(coreConfig))
  val id_ex = Module(new ID_EX(coreConfig))
  val exu = Module(new EX(coreConfig))
  val ex_mem = Module(new EX_MEM(coreConfig))
  val memu = Module(new MEM(coreConfig))
  val mem_wb = Module(new MEM_WB(coreConfig))
  // val wbu = Module(new WB(coreConfig))

  val ram_mux = Module(new RamMux(coreConfig))
  ram_mux.io.ram_io <> io.ram

  val stall = Wire(Bool())
  val flush = Wire(Bool())

  Debug("-----------------------------------\n")

  stall := ex_mem.io.stall
  flush := exu.io.out.prediction_failure

  pc := coreConfig.InitialPC.U
  when(ifu.io.out.valid) {
    pc := idu.io.out.predicted_pc
  }
  when(exu.io.out.prediction_failure) {
    pc := exu.io.out.jump_pc
  }

  Debug(stall, "!!!STALL!!!\n")
  Debug(flush, "!!!FLUSH!!!\n")

  ifu.io.stall := stall
  ifu.io.flush := flush
  ifu.io.in.pc := pc
  ifu.io.if_io <> ram_mux.io.if_io
  // Debug(
  //   ifu.io.if_io.en,
  //   "IF: fetch pc=0x%x 0x%x\n",
  //   ifu.io.if_io.addr,
  //   ifu.io.if_io.data.foldLeft(0.U(1.W))(Cat(_, _))
  // )

  idu.io.in.pc := ifu.io.out.pc
  idu.io.in.instr := ifu.io.out.instr
  idu.io.reg_io <> regs.io.rport
  Debug(
    ifu.io.out.valid,
    "ID in: pc=0x%x 0x%x\n",
    idu.io.in.pc,
    idu.io.in.instr
  )
  Debug(
    ifu.io.out.valid,
    "ID out: predicted_pc=0x%x fn=%d rs1=%d rs2=%d mem=%d%d wb=%d\n",
    idu.io.out.predicted_pc,
    idu.io.out.ex.fn,
    idu.io.out.ex.rs1,
    idu.io.out.ex.rs2,
    idu.io.out.mem.en,
    idu.io.out.mem.rw,
    idu.io.out.write_back.rd
  )

  id_ex.io.stall := stall
  id_ex.io.in_valid := ifu.io.out.valid
  id_ex.io.in := idu.io.out

  exu.io.in_valid := id_ex.io.out_valid
  exu.io.in := id_ex.io.out
  Debug(
    id_ex.io.out_valid,
    "EX in: predicted_pc=0x%x fn=%d rs1=%d rs2=%d\n",
    exu.io.in.predicted_pc,
    exu.io.in.ex.fn,
    exu.io.in.ex.rs1,
    exu.io.in.ex.rs2
  )
  Debug(
    id_ex.io.out_valid,
    "EX out: mem=%d%d %x; write %d=0x%x\n",
    exu.io.out.mem.en,
    exu.io.out.mem.rw,
    exu.io.out.mem.addr,
    exu.io.out.write_back.rd,
    exu.io.out.write_back.data
  )

  // ex_mem.io.stall := stall
  ex_mem.io.in_valid := id_ex.io.out_valid
  ex_mem.io.in := exu.io.out
  exu.io.forward(0) <> ex_mem.io.out.write_back
  ex_mem.io.out.mem_rdata := memu.io.out.rdata

  memu.io.in.mem := ex_mem.io.out.mem
  memu.io.in.write_back := ex_mem.io.out.write_back
  memu.io.mem_io <> ram_mux.io.mem_io
  Debug(
    memu.io.in.mem.en,
    "MEM in: mem=%d width=%d %x %x\n",
    memu.io.in.mem.rw,
    memu.io.in.mem.wWidth,
    memu.io.in.mem.addr,
    memu.io.in.mem.wdata
  )
  // Debug(
  //   "MEM %x read %x, mask %x, write %x\n",
  //   mem.io.rwport.addr << 3,
  //   mem.io.rwport.rdata.reverse.foldLeft(0.U(1.W))(Cat(_, _)),
  //   mem.io.rwport.wmask.reverse.foldLeft(0.U(1.W))(Cat(_, _)),
  //   mem.io.rwport.wdata.reverse.foldLeft(0.U(1.W))(Cat(_, _))
  // )

  mem_wb.io.stall := stall
  mem_wb.io.in := memu.io.out

  regs.io.wport.wen := true.B
  regs.io.wport.waddr := mem_wb.io.out.write_back.rd
  regs.io.wport.wdata := mem_wb.io.out.write_back.data
  exu.io.forward(1) <> mem_wb.io.out.write_back
  Debug(
    mem_wb.io.out.write_back.rd =/= 0.U,
    "WB in: reg%d=%x\n",
    mem_wb.io.out.write_back.rd,
    mem_wb.io.out.write_back.data
  )

  if (coreConfig.DebugPin) {
    io.debug.get.reg := regs.io.debug.get.reg
    io.debug.get.if_pc := ifu.io.out.pc
    io.debug.get.if_instr := ifu.io.out.instr
  }
}
