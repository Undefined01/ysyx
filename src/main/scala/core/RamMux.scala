package rvcore

import chisel3._
import chisel3.util._

import device.RAM.{RamIo, PortDriver}

class RamMux(coreConfig: CoreConfig) extends Module {
  val io = IO(new Bundle {
    val if_io = new Bundle {
      val valid = Output(Bool())
      val raddr = Input(UInt(coreConfig.XLEN.W))
      val rdata = Output(UInt(coreConfig.XLEN.W))
    }
    val mem_io = new Bundle {
      val en = Input(Bool())
      val rw = Input(Bool())
      val unsigned = Input(Bool())
      val wWidth = Input(UInt(3.W))
      val addr = Input(UInt(coreConfig.XLEN.W))
      val rdata = Output(UInt(coreConfig.XLEN.W))
      val wdata = Input(UInt(coreConfig.XLEN.W))
    }
    val ram_io = new RamIo(coreConfig.XLEN, coreConfig.XLEN / 8)
  })

  val valid = RegNext(true.B, false.B)
  val driver = Module(new PortDriver(coreConfig.XLEN, coreConfig.XLEN / 8))

  io.ram_io.en := true.B
  io.ram_io.raddr := driver.io.ram_ctrl.raddr
  driver.io.ram_ctrl.rdata := io.ram_io.rdata
  io.ram_io.wen := io.mem_io.en && io.mem_io.rw
  io.ram_io.waddr := driver.io.ram_ctrl.waddr
  io.ram_io.wmask := driver.io.ram_ctrl.wmask
  io.ram_io.wdata := driver.io.ram_ctrl.wdata

  driver.io.waddr := io.mem_io.addr
  driver.io.wWidth := io.mem_io.wWidth
  driver.io.wdata := io.mem_io.wdata
  io.if_io.rdata := driver.io.rdata
  io.mem_io.rdata := driver.io.rdata

  when(io.mem_io.en && !io.mem_io.rw) {
    driver.io.raddr := io.mem_io.addr
    driver.io.rUnsigned := io.mem_io.unsigned
    driver.io.rWidth := io.mem_io.wWidth
    io.if_io.valid := false.B
  }.otherwise {
    driver.io.raddr := io.if_io.raddr
    driver.io.rUnsigned := true.B
    driver.io.rWidth := 2.U // 4 Bytes
    io.if_io.valid := true.B
  }
}
