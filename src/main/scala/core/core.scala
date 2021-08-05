package rvcore

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import utils.Logger._

import device.RAM.RamIo

class RvCore(implicit c: CoreConfig) extends Module {
  val io = IO(new Bundle {
    val ram = new RamIo(c.XLEN, c.XLEN / 8)
    val debug =
      if (c.DebugPort) Some(new Bundle {
        val reg = Output(Vec(32, UInt(c.XLEN.W)))
      })
      else None
  })

  val pc = Wire(UInt(c.XLEN.W))

  val regs = Module(new RegFile)

  val ifu = Module(new IF)
  val idu = Module(new ID)
  val id_ex = Module(new ID_EX)
  val exu = Module(new EX)
  val ex_mem = Module(new EX_MEM)
  val memu = Module(new MEM)
  val mem_wb = Module(new MEM_WB)
  val wbu = Module(new WB)

  val ram_mux = Module(new RamMux)
  ram_mux.io.ram_io <> io.ram

  val stall = Wire(Bool())
  val flush = Wire(Bool())

  Debug("-----------------------------------\n")

  stall := ex_mem.io.stall
  flush := exu.io.out.prediction_failure

  pc := c.InitialPC.U
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
  if (c.DebugPort) {
    BoringUtils.addSource(ifu.io.out.pc, "IF_pc")
    BoringUtils.addSource(ifu.io.out.instr, "IF_instr")
  }
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
    idu.io.out.wb.rd
  )
  Warn(
    ifu.io.out.valid && idu.io.out.has_error,
    "!!! DECODE ERROR !!!  pc=%x instr=%x\n",
    idu.io.in.pc,
    idu.io.in.instr
  )

  id_ex.io.stall := stall
  id_ex.io.in_valid := ifu.io.out.valid
  id_ex.io.in := idu.io.out

  exu.io.in_valid := id_ex.io.out_valid
  exu.io.in := id_ex.io.out
  exu.io.forward(0) := ex_mem.io.out.wb
  exu.io.forward(1) := mem_wb.io.out.wb
  Debug(
    id_ex.io.out_valid,
    "EX in: pc=0x%x fn=%d rs1=%d rs2=%d\n",
    exu.io.in.commit.pc,
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
    exu.io.out.wb.rd,
    exu.io.out.wb.data
  )

  // ex_mem.io.stall := stall
  ex_mem.io.in_valid := id_ex.io.out_valid
  ex_mem.io.in := exu.io.out
  ex_mem.io.out.mem_rdata := memu.io.out.rdata

  memu.io.in.mem := ex_mem.io.out.mem
  memu.io.mem_io <> ram_mux.io.mem_io
  Debug(
    ex_mem.io.out_valid,
    "MEM in: pc=0x%x mem=%d%d width=%d %x %x\n",
    ex_mem.io.out.commit.pc,
    memu.io.in.mem.en,
    memu.io.in.mem.rw,
    memu.io.in.mem.wWidth,
    memu.io.in.mem.addr,
    memu.io.in.mem.wdata
  )

  mem_wb.io.stall := stall
  mem_wb.io.in_valid := ex_mem.io.out_valid
  mem_wb.io.in.commit := ex_mem.io.out.commit
  mem_wb.io.in.wb := ex_mem.io.out.wb

  wbu.io.stall := stall
  wbu.io.in_valid := mem_wb.io.out_valid
  wbu.io.in := mem_wb.io.out
  regs.io.wb := wbu.io.reg_wb
  Debug(
    mem_wb.io.out_valid,
    "WB in: pc=%x reg%d=%x\n",
    mem_wb.io.out.commit.pc,
    mem_wb.io.out.wb.rd,
    mem_wb.io.out.wb.data
  )

  if (c.DebugPort) {
    io.debug.get.reg := regs.io.debug.get.reg
  }
}
