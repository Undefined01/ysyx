package rvcore

import chisel3._
import chisel3.util._
import device._
import utils.Logger._

object Clint {
  object AddrOffset {
    val mtime = 0xbff8
    val mtimecmp = 0x4000
  }
}

class Clint(implicit axi_config: AXI4Config) extends Module {
  val io = IO(new Bundle {
    val axi = Flipped(new AXI4Bundle)
    val out = new Bundle {
      val timer_intr = Output(Bool())
    }
  })

  val mtime = RegInit(0.U(64.W))
  mtime := mtime + 1.U
  val mtimecmp = RegInit(0.U(64.W))

  io.axi.flippedDefault()

  val rState = RegInit(0.U(1.W))
  val rAddr = RegInit(0.U(32.W))
  switch(rState) {
    is(0.U) {
      when(io.axi.ar.valid) {
        io.axi.ar.ready := true.B
        rAddr := io.axi.ar.bits.addr
        rState := 1.U
      }
    }
    is(1.U) {
      io.axi.r.valid := true.B
      io.axi.r.bits.last := true.B
      switch(rAddr(31, 3)) {
        is((Clint.AddrOffset.mtime >> 3).U) {
          io.axi.r.bits.data := mtime
        }
        is((Clint.AddrOffset.mtimecmp >> 3).U) {
          io.axi.r.bits.data := mtimecmp
        }
      }
      when(io.axi.r.ready) {
        rState := 0.U
      }
    }
  }

  val wState = RegInit(0.U(2.W))
  val wAddr = RegInit(0.U(32.W))
  val wData = RegInit(0.U(64.W))
  switch(wState) {
    is(0.U) {
      when(io.axi.aw.valid) {
        io.axi.aw.ready := true.B
        wAddr := io.axi.aw.bits.addr
        wState := 1.U
        when(io.axi.w.valid) {
          io.axi.w.ready := true.B
          wData := io.axi.w.bits.data
          wState := 2.U
        }
      }
    }
    is(1.U) {
      when(io.axi.w.valid) {
        io.axi.w.ready := true.B
        wData := io.axi.w.bits.data
        wState := 2.U
      }
    }
    is(2.U) {
      io.axi.b.valid := true.B
      switch(wAddr(31, 3)) {
        is((Clint.AddrOffset.mtimecmp >> 3).U) {
          mtimecmp := wData
        }
      }
      when(io.axi.b.ready) {
        wState := 0.U
      }
    }
  }

  io.out.timer_intr := mtime >= mtimecmp
}
