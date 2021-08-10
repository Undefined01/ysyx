package rvcore

import chisel3._
import chisel3.util._
import device._
import utils._
import utils.Logger.Debug

class EX_WB(implicit c: CoreConfig) extends Module {
  val io = IO(new Bundle {
    val stall = Output(Bool())

    val in_valid = Input(Bool())
    val in = new Bundle {
      val commit = Input(new CommitIO)
      val mem = Input(new MemIO)
      val wb = Input(new WriteBackIO)
    }

    val axi = new AXI4Bundle

    val out_valid = Output(Bool())
    val out = new Bundle {
      val commit = Output(new CommitIO)
      val wb = Output(new WriteBackIO)
    }
  })

  val state = RegInit(0.U(2.W))

  io.out_valid := RegEnable(io.in_valid, false.B, !io.stall)
  io.out.commit := RegEnable(io.in.commit, !io.stall)
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
  val mask = Cat(strb.asBools.map { x => Fill(8, x) })
  switch(state) {
    // Idle
    is(0.U) {
      when(io.in.mem.en) {
        io.stall := true.B
        when(io.in.mem.rw) {
          printf("Start writing mem %x\n", io.in.mem.addr)
          io.axi.aw.valid := true.B
          io.axi.aw.addr := io.in.mem.addr
          when(io.axi.aw.ready) {
            state := 1.U
          }
        }.otherwise {
          printf("Start reading mem %x\n", io.in.mem.addr)
          io.axi.ar.valid := true.B
          io.axi.ar.addr := io.in.mem.addr
          io.axi.ar.len := 0.U
          io.axi.ar.size := io.in.mem.wWidth
          io.axi.ar.burst := 0.U
          when(io.axi.ar.ready) {
            printf("Reading data %x\n", io.axi.r.data)
            state := 3.U
          }
        }
      }
    }
    // Write data
    is(1.U) {
      printf("Writing data %x\n", io.in.mem.wdata)
      io.stall := true.B
      io.axi.w.valid := true.B
      io.axi.w.strb := strb << subaddr
      io.axi.w.data := io.in.mem.wdata << (subaddr * 8.U)
      io.axi.w.last := true.B
      when(io.axi.w.ready) {
        state := 2.U
        when(io.axi.b.valid) {
          printf("Response received\n")
          io.stall := false.B
          state := 0.U
        }
      }
    }
    // Write response
    is(2.U) {
      io.stall := true.B
      when(io.axi.b.valid) {
        printf("Response received\n")
        io.stall := false.B
        state := 0.U
      }
    }
    // Read data
    is(3.U) {
      printf("Reading data %x\n", io.axi.r.data)
      io.stall := true.B
      val rdata = (io.axi.r.data >> (subaddr * 8.U)) & mask
      val wbdata = Mux(
        io.in.mem.unsigned,
        rdata,
        VecInit((0 to log2Ceil(dataBytes)).map { x =>
          SignExt(rdata((1 << x) * 8 - 1, 0), dataBytes * 8)
        })(io.in.mem.wWidth)
      )
      when(io.axi.r.valid) {
        state := 0.U
        io.stall := false.B
        wb.data := wbdata
      }
    }
  }
  when(io.in.mem.en) {
    printf("MEM enabled, state=%d, stall=%d\n", state, io.stall);
  }
}
