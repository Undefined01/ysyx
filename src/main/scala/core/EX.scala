package rvcore

import chisel3._
import chisel3.util._
import utils.Logger.Debug

class EX(implicit c: CoreConfig) extends Module {
  val io = IO(new Bundle {
    val in_valid = Input(Bool())
    val in = new Bundle {
      val predicted_pc = Input(UInt(c.XLEN.W))
      val commit = Input(new CommitIO)
      val ex = Input(new ExIO)
      val mem = Input(new MemIO)
      val wb = Input(new WriteBackIO)
    }

    val forward = Vec(1, Input(new WriteBackIO))
    val timer_intr = Input(Bool())

    val out_valid = Output(Bool())
    val out = new Bundle {
      val jump = Output(new Bundle {
        val valid = Bool()
        val pc = UInt(c.XLEN.W)
      })
      val commit = Output(new CommitIO)
      val mem = Output(new MemIO)
      val wb = Output(new WriteBackIO)
    }
  })

  def handleForwarding(reg: UInt, data: UInt): UInt = {
    val res = WireDefault(data)
    when(reg =/= 0.U) {
      when(reg === io.forward(0).rd) {
        res := io.forward(0).data
      }
    }
    res
  }

  val alu = Module(new Alu)
  alu.io.in.fn := io.in.ex.fn
  alu.io.in.is_op32 := io.in.ex.is_op32
  val rop1 = handleForwarding(io.in.ex.rs1, io.in.ex.op1)
  val rop2 = handleForwarding(io.in.ex.rs2, io.in.ex.op2)
  alu.io.in.op1 := rop1
  alu.io.in.op2 := Mux(io.in.ex.use_imm, io.in.ex.imm, rop2)

  io.out_valid := io.in_valid
  io.out.mem.en := io.in.mem.en
  io.out.mem.rw := io.in.mem.rw
  io.out.mem.unsigned := io.in.mem.unsigned
  io.out.mem.wWidth := io.in.mem.wWidth
  io.out.mem.addr := alu.io.out
  io.out.mem.wdata := rop2

  io.out.wb.rd := io.in.wb.rd
  io.out.wb.data := alu.io.out

  io.out.commit := io.in.commit

  io.out.jump.valid := false.B
  io.out.jump.pc := DontCare
  when(io.in_valid && io.in.ex.is_jump) {
    io.out.jump.pc := alu.io.out
    io.out.jump.valid := io.in.predicted_pc =/= io.out.jump.pc
    io.out.wb.data := io.in.ex.imm
  }
  when(io.in_valid && io.in.ex.is_branch) {
    io.out.jump.pc := Mux(
      alu.io.out(0).asBool,
      io.in.ex.imm,
      io.in.commit.pc + 4.U
    )
    io.out.jump.valid := io.in.predicted_pc =/= io.out.jump.pc
  }

  val csr = Module(new Csr)
  val csr_en = io.in_valid && io.in.ex.is_csr
  csr.io.en := csr_en
  csr.io.csrfn := io.in.ex.csrfn
  csr.io.csr_number := io.in.ex.imm
  csr.io.op := Mux(io.in.ex.use_imm, io.in.ex.rs1, rop1)
  when(csr_en) {
    io.out.wb.data := csr.io.res
  }
  io.out.commit.skip := csr_en && csr.io.skip

  csr.io.trap.is_timerintr := io.timer_intr
  when(io.timer_intr && csr.io.trap.jump.valid) {
    io.out_valid := false.B
    io.out.commit.event.intrNO := 7.U
    io.out.commit.event.exceptionPC := io.in.commit.pc
    io.out.jump := csr.io.trap.jump
  }

  val trap_en = io.in_valid && io.in.ex.is_trap
  csr.io.trap.is_ecall := false.B
  csr.io.trap.is_mret := false.B
  csr.io.trap.pc := io.in.commit.pc
  when(trap_en) {
    switch(io.in.ex.rs2) {
      is("b00000".U) {
        csr.io.trap.is_ecall := true.B
        io.out.jump := csr.io.trap.jump
      }
      is("b00010".U) {
        csr.io.trap.is_mret := true.B
        io.out.jump := csr.io.trap.jump
      }
    }
  }
}
