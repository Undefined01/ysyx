package rvcore

import chisel3._
import chisel3.util._

class WB(implicit c: CoreConfig) extends Module {
  val io = IO(new Bundle {
    val stall = Input(Bool())
    val in_valid = Input(Bool())
    val in = new Bundle {
      val pc = Input(UInt(c.XLEN.W))
      val wb = Flipped(new WriteBackIO)
    }
    val reg_wb = new WriteBackIO
  })

  io.reg_wb := io.in.wb
  io.reg_wb.set_valid(io.in_valid)

  if (c.DiffTest) {
    val commit = Module(new difftest.DifftestInstrCommit)
    commit.io.clock := clock
    commit.io.coreid := c.CoreId.U
    commit.io.index := 0.U

    commit.io.valid := io.in_valid && !io.stall
    commit.io.pc := io.in.pc
    commit.io.instr := DontCare // RegNext(io.in.instr)
    commit.io.skip := false.B
    commit.io.isRVC := false.B
    commit.io.scFailed := false.B
    commit.io.wen :=
      io.in_valid && io.in.wb.rd =/= 0.U
    commit.io.wdata := io.in.wb.data
    commit.io.wdest := io.in.wb.rd

    // val trap = Module(new DifftestTrapEvent)
    // trap.io.clock    := clock
    // trap.io.coreid   := c.CoreId.U
    // trap.io.valid    := nutcoretrap
    // trap.io.code     := 0.U // GoodTrap
    // trap.io.pc       := io.in.pc
    // trap.io.cycleCnt := DontCare
    // trap.io.instrCnt := DontCare
  }
}
