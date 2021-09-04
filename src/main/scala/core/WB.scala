package rvcore

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

class WB(implicit c: CoreConfig) extends Module {
  val io = IO(new Bundle {
    val stall = Input(Bool())
    val in_valid = Input(Bool())
    val in = new Bundle {
      val commit = Input(new CommitIO)
      val wb = Input(new WriteBackIO)
    }
    val reg_wb = Output(new WriteBackIO)
  })

  io.reg_wb := io.in.wb
  io.reg_wb.set_valid(io.in_valid)

  if (c.DiffTest) {
    val cycleCnt = RegInit(0.U(32.W))
    cycleCnt := cycleCnt + 1.U
    val instrCnt = RegInit(0.U(32.W))
    when(io.in_valid && !io.stall) {
      instrCnt := instrCnt + 1.U
    }

    val reg_a0 = WireInit(0.U(64.W))
    BoringUtils.addSink(reg_a0, "RegFile_a0")

    val commit = Module(new difftest.DifftestInstrCommit)
    commit.io.clock := clock
    commit.io.coreid := c.CoreId.U
    commit.io.index := 0.U

    commit.io.valid := io.in_valid && !io.stall
    commit.io.pc := io.in.commit.pc
    commit.io.instr := io.in.commit.instr
    commit.io.skip := false.B
    commit.io.isRVC := false.B
    commit.io.scFailed := false.B
    commit.io.wen :=
      io.in_valid && io.in.wb.rd =/= 0.U
    commit.io.wdata := io.in.wb.data
    commit.io.wdest := io.in.wb.rd

    val trap = Module(new difftest.DifftestTrapEvent)
    trap.io.clock := clock
    trap.io.coreid := c.CoreId.U
    trap.io.valid := io.in_valid && io.in.commit.instr === "h0000006b".U
    trap.io.code := reg_a0
    trap.io.pc := io.in.commit.pc
    trap.io.cycleCnt := cycleCnt
    trap.io.instrCnt := instrCnt

    when(io.in_valid && io.in.commit.is_putch) {
      commit.io.skip := true.B
      printf("%c", reg_a0)
    }
    when(io.in.commit.skip) {
      commit.io.skip := true.B
    }
  }
}
