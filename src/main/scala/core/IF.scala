package rvcore

import chisel3._
import chisel3.util._
import utils.Logger._
import device._

class IF(implicit c: CoreConfig, axi_config: AXI4Config) extends Module {
  val io = IO(new Bundle {
    val stall = Input(Bool())
    val flush = Input(Bool())

    val in = new Bundle {
      val next_pc = Input(Valid(UInt(c.XLEN.W)))
    }

    // IO to fetch instruction from memory
    val axi = new AXI4Bundle

    val out = new Bundle {
      val valid = Output(Bool())
      val pc = Output(UInt(c.XLEN.W))
      val instr = Output(UInt(c.InstrLen.W))
    }
  })

  val cache = Reg(UInt(c.XLEN.W))
  val cacheIdx = Reg(UInt((c.XLEN - 3).W))

  val next_pc = Mux(
    io.in.next_pc.valid,
    io.in.next_pc.bits,
    RegEnable(io.in.next_pc.bits, c.InitialPC.U, io.in.next_pc.valid)
  )
  val pcIdx = next_pc(c.XLEN - 1, 3)
  val cacheMiss = cacheIdx =/= pcIdx

  val state = RegInit(0.U(2.W))
  val pcFetchIdx = Reg(UInt((c.XLEN - 3).W))
  io.axi.default()
  switch(state) {
    is(0.U) {
      when(cacheMiss) {
        Debug("Cache missed for %x\n", next_pc)
        io.axi.ar.valid := true.B
        io.axi.ar.bits.addr := next_pc
        io.axi.ar.bits.id := 0.U
        io.axi.ar.bits.len := 0.U
        io.axi.ar.bits.size := 3.U
        io.axi.ar.bits.burst := 1.U
        pcFetchIdx := pcIdx
        when(io.axi.ar.ready) {
          state := 1.U
        }
      }
    }
    is(1.U) {
      io.axi.r.ready := true.B
      when(io.axi.r.valid) {
        state := 0.U
        cacheIdx := pcFetchIdx
        cache := io.axi.r.bits.data
        Debug("Cache loaded for %x: %x\n", Cat(pcFetchIdx, 0.U(3.W)), io.axi.r.bits.data)
      }
    }
  }

  io.out.valid := !io.flush && !RegEnable(cacheMiss, enable = !io.stall)
  io.out.pc := RegEnable(next_pc, enable = !io.stall)
  io.out.instr := Mux(io.out.pc(2), cache(63, 32), cache(31, 0))
}
