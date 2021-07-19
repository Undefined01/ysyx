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
    val mem =
      Flipped(new Memory.ReadWritePort(coreConfig.XLEN, coreConfig.XLEN / 8))
    val out = new Bundle {
      val rdata = Output(UInt(coreConfig.XLEN.W))
      val write_back = new Bundle {
        val rd = Output(UInt(coreConfig.RegAddrWidth.W))
        val data = Output(UInt(coreConfig.XLEN.W))
      }
    }
  })

  val mem_driver = Module(
    new Memory.PortDriver(coreConfig.XLEN, coreConfig.XLEN / 8)
  )
  mem_driver.io.in.addr := io.in.mem.addr
  mem_driver.io.in.unsigned := io.in.mem.unsigned
  mem_driver.io.in.wWidth := io.in.mem.wWidth
  mem_driver.io.in.wdata := io.in.mem.wdata

  io.mem.en := io.in.mem.en
  io.mem.rw := io.in.mem.rw
  io.mem.addr := mem_driver.io.mem_ctrl.addr
  mem_driver.io.mem_ctrl.rdata := io.mem.rdata
  io.mem.wmask := mem_driver.io.mem_ctrl.wmask
  io.mem.wdata := mem_driver.io.mem_ctrl.wdata

  io.out.rdata := mem_driver.io.out.rdata
  io.out.write_back := io.in.write_back
}
