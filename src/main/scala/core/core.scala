package rvcore

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import device._
import utils.Logger._

import device.RAM.RamIo

class RvCore(implicit c: CoreConfig, axi_config: AXI4Config) extends Module {
  Debug("-----------------------------------\n")
  val io = IO(new Bundle {})

  val regs = Module(new RegFile)

  val ifu = Module(new IF)
  val idu = Module(new ID)
  val id_ex = Module(new ID_EX)
  val exu = Module(new EX)
  val ex_wb = Module(new EX_WB)
  val wbu = Module(new WB)
  
  val axi_arbiter = Module(new AXI4Arbiter)
  AXI4RAM(clock, reset, axi_arbiter.io.slavePort(0))

  val stall = Wire(Bool())
  val flush = Wire(Bool())

  stall := ex_wb.io.stall
  flush := exu.io.out.prediction_failure

  val next_pc = Wire(Valid(UInt(c.XLEN.W)))
  next_pc.valid := false.B
  next_pc.bits := DontCare
  when(ifu.io.out.valid) {
    next_pc.valid := true.B
    next_pc.bits := idu.io.out.predicted_pc
  }
  when(exu.io.out.prediction_failure) {
    next_pc.valid := true.B
    next_pc.bits := exu.io.out.jump_pc
  }

  Debug(stall, "!!!STALL!!!\n")
  Debug(flush, "!!!FLUSH!!!\n")

  ifu.io.stall := stall
  ifu.io.flush := flush
  ifu.io.in.next_pc := next_pc
  axi_arbiter.io.masterPort(1) <> ifu.io.axi

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
  exu.io.forward(0) := ex_wb.io.out.wb
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

  ex_wb.io.in_valid := id_ex.io.out_valid
  ex_wb.io.in := exu.io.out
  axi_arbiter.io.masterPort(0) <> ex_wb.io.axi

  wbu.io.stall := stall
  wbu.io.in_valid := ex_wb.io.out_valid
  wbu.io.in := ex_wb.io.out
  regs.io.wb := wbu.io.reg_wb
  Debug(
    ex_wb.io.out_valid,
    "WB in: pc=%x reg%d=%x\n",
    ex_wb.io.out.commit.pc,
    ex_wb.io.out.wb.rd,
    ex_wb.io.out.wb.data
  )
}
