package rvcore

import chisel3._
import chisel3.util._
import utils.Logger.Debug

class MEM(coreConfig: CoreConfig) extends Module {
  val io = IO(new Bundle {
    val in = new Bundle {
      val mem = new Bundle {
        val en = Input(Bool())
        val rw = Input(Bool())
        val unsigned = Input(Bool())
        val wWidth = Input(UInt(3.W))
        val addr = Input(UInt(coreConfig.XLEN.W))
        val wdata = Input(UInt(coreConfig.XLEN.W))
      }
      val write_back = new Bundle {
        val rd = Input(UInt(coreConfig.RegAddrWidth.W))
        val data = Input(UInt(coreConfig.XLEN.W))
      }
    }
    val mem_io = new Bundle {
      val en = Output(Bool())
      val rw = Output(Bool())
      val unsigned = Output(Bool())
      val wWidth = Output(UInt(3.W))
      val addr = Output(UInt(coreConfig.XLEN.W))
      val rdata = Input(UInt(coreConfig.XLEN.W))
      val wdata = Output(UInt(coreConfig.XLEN.W))
    }
    val out = new Bundle {
      val rdata = Output(UInt(coreConfig.XLEN.W))
      val write_back = new Bundle {
        val rd = Output(UInt(coreConfig.RegAddrWidth.W))
        val data = Output(UInt(coreConfig.XLEN.W))
      }
    }
  })

  io.mem_io.en := io.in.mem.en
  io.mem_io.rw := io.in.mem.rw
  io.mem_io.unsigned := io.in.mem.unsigned
  io.mem_io.wWidth := io.in.mem.wWidth
  io.mem_io.addr := io.in.mem.addr
  io.mem_io.wdata := io.in.mem.wdata

  io.out.rdata := io.mem_io.rdata
  io.out.write_back := io.in.write_back
}
