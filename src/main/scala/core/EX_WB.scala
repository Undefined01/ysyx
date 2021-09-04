package rvcore

import chisel3._
import chisel3.util._
import device._
import utils._
import utils.Logger.Debug

class EX_WB(implicit c: CoreConfig, axi_config: AXI4Config) extends Module {
  val io = IO(new Bundle {
    val stall = Output(Bool())

    val in_valid = Input(Bool())
    val in = new Bundle {
      val commit = Input(new CommitIO)
      val mem = Input(new MemIO)
      val wb = Input(new WriteBackIO)
    }

    val axi = new AXI4Bundle
    val is_mmio = Input(Bool())

    val out_valid = Output(Bool())
    val out = new Bundle {
      val commit = Output(new CommitIO)
      val wb = Output(new WriteBackIO)
    }
  })

  val state = RegInit(0.U(2.W))

  io.out_valid := RegEnable(io.in_valid, false.B, !io.stall)
  val out_commit = RegEnable(io.in.commit, !io.stall)
  when(io.is_mmio) {
    out_commit.skip := true.B
  }
  io.out.commit := out_commit
  val wb = RegEnable(io.in.wb, !io.stall)
  io.out.wb := wb
  io.out.wb.set_valid(io.out_valid)

  io.stall := false.B

  io.axi.default()
  val dataBytes = c.XLEN / 8
  val subaddr = io.in.mem.addr(log2Ceil(dataBytes) - 1, 0)
  val strb = VecInit((0 to log2Ceil(dataBytes)).map { x =>
    Cat((0 until dataBytes).reverse.map { y =>
      (y < (1 << x)).B.asUInt
    })
  })(io.in.mem.wWidth)
  val mask = Cat(strb.asBools.reverse.map { x => Fill(8, x) })
  switch(state) {
    // Idle
    is(0.U) {
      when(io.in.mem.en) {
        io.stall := true.B
        when(io.in.mem.rw) {
          io.axi.aw.valid := true.B
          io.axi.aw.bits.addr := io.in.mem.addr
          when(io.axi.aw.ready) {
            state := 1.U
          }
          io.axi.w.valid := true.B
          io.axi.w.bits.strb := strb << subaddr
          io.axi.w.bits.data := io.in.mem.wdata << (subaddr * 8.U)
          io.axi.w.bits.last := true.B
          when(io.axi.w.ready) {
            state := 2.U
          }
        }.otherwise {
          io.axi.ar.valid := true.B
          io.axi.ar.bits.addr := io.in.mem.addr
          io.axi.ar.bits.len := 0.U
          io.axi.ar.bits.size := io.in.mem.wWidth
          io.axi.ar.bits.burst := 0.U
          when(io.axi.ar.ready) {
            state := 3.U
          }
        }
      }
    }
    // Write data
    is(1.U) {
      io.stall := true.B
      io.axi.w.valid := true.B
      io.axi.w.bits.strb := strb << subaddr
      io.axi.w.bits.data := io.in.mem.wdata << (subaddr * 8.U)
      io.axi.w.bits.last := true.B
      when(io.axi.w.ready) {
        state := 2.U
      }
    }
    // Write response
    is(2.U) {
      io.stall := true.B
      io.axi.b.ready := true.B
      when(io.axi.b.valid) {
        io.stall := false.B
        state := 0.U
      }
    }
    // Read data
    is(3.U) {
      io.stall := true.B
      io.axi.r.ready := true.B
      val axidata = (io.axi.r.bits.data >> (subaddr * 8.U)) & mask
      val data = Mux(
        io.in.mem.unsigned,
        axidata,
        VecInit((0 to log2Ceil(dataBytes)).map { x =>
          SignExt(axidata((1 << x) * 8 - 1, 0), dataBytes * 8)
        })(io.in.mem.wWidth)
      )
      when(io.axi.r.valid) {
        state := 0.U
        io.stall := false.B
        wb.data := data
      }
    }
  }
}
