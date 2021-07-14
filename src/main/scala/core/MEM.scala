package core

import chisel3._
import chisel3.util._
import utils.Logger.Debug

class MEM(coreConfig: CoreConfig) extends Module {
  val io = IO(new Bundle {
    val in_ready = Output(Bool())
    val in = new Bundle {
      val valid = Input(Bool())
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
    val out = new Bundle {
    val valid = Output(Bool())
      val mem =
        Flipped(new Memory.ReadWritePort(coreConfig.XLEN, coreConfig.XLEN / 8))
      val write_back = new Bundle {
        val rd = Output(UInt(coreConfig.RegAddrWidth.W))
        val data = Output(UInt(coreConfig.XLEN.W))
      }
    }
  })

  val state = RegInit(0.U(1.W))

  val mem_driver = Module(
    new Memory.PortDriver(coreConfig.XLEN, coreConfig.XLEN / 8)
  )
  mem_driver.io.in.addr := io.in.mem.addr
  mem_driver.io.in.unsigned := io.in.mem.unsigned
  mem_driver.io.in.wWidth := io.in.mem.wWidth
  mem_driver.io.in.wdata := io.in.mem.wdata

  io.out.mem.en := io.in.valid && io.in.mem.en
  io.out.mem.rw := io.in.mem.rw
  io.out.mem.addr := mem_driver.io.mem_ctrl.addr
  mem_driver.io.mem_ctrl.rdata := io.out.mem.rdata
  io.out.mem.wmask := mem_driver.io.mem_ctrl.wmask
  io.out.mem.wdata := mem_driver.io.mem_ctrl.wdata

  io.out.write_back.rd := io.in.write_back.rd
  io.out.write_back.data := io.in.write_back.data

  io.in_ready := true.B
  io.out.valid := true.B
  when(state === 0.U) {
    when(io.in.mem.en && !io.in.mem.rw) {
      state := 1.U
      io.in_ready := false.B
      io.out.valid := false.B
      Debug(io.out.mem.en, "MEM fetch %x\n", io.in.mem.addr)
    }
  }.elsewhen(state === 1.U) {
    state := 0.U
    io.out.mem.en := false.B
    io.out.write_back.data := mem_driver.io.out.rdata
    Debug(
      "MEM %x got %x\n",
      io.in.mem.addr,
      mem_driver.io.out.rdata
    )
  }
}
