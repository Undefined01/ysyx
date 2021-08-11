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
    val cycle = RegInit(0.U(32.W))
    cycle := cycle + 1.U

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
    trap.io.cycleCnt := cycle
    trap.io.instrCnt := cycle

    val csr = Module(new difftest.DifftestCSRState)
    csr.io.clock := clock
    csr.io.coreid := c.CoreId.U
    csr.io.mstatus := 0.U
    csr.io.mcause := 0.U
    csr.io.mepc := 0.U
    csr.io.sstatus := 0.U
    csr.io.scause := 0.U
    csr.io.sepc := 0.U
    csr.io.satp := 0.U
    csr.io.mip := 0.U
    csr.io.mie := 0.U
    csr.io.mscratch := 0.U
    csr.io.sscratch := 0.U
    csr.io.mideleg := 0.U
    csr.io.medeleg := 0.U
    csr.io.mtval := 0.U
    csr.io.stval := 0.U
    csr.io.mtvec := 0.U
    csr.io.stvec := 0.U
    csr.io.priviledgeMode := 0.U

    when(io.in.commit.is_putch) {
      commit.io.skip := true.B
      printf("%c", reg_a0)
    }
    when(io.in.commit.is_csrskip) {
      commit.io.skip := true.B
    }
  }
}
