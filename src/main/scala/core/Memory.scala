package core

import chisel3._
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFile

object Memory {
  class ReadPort(addrWidth: Int, dataBytes: Int) extends Bundle {
    val en = Input(Bool())
    val addr = Input(UInt(addrWidth.W))
    val data = Output(Vec(dataBytes, UInt(8.W)))
  }
  class ReadWritePort(addrWidth: Int, dataBytes: Int) extends Bundle {
    val en = Input(Bool())
    val rw = Input(Bool())
    val addr = Input(UInt(addrWidth.W))
    val rdata = Output(Vec(dataBytes, UInt(8.W)))
    val wmask = Input(Vec(dataBytes, Bool()))
    val wdata = Input(Vec(dataBytes, UInt(8.W)))
  }
}

class Memory(
    addrWidth: Int,
    dataBytes: Int,
    depth: Int,
    memoryFile: String = ""
) extends Module {
  val io = IO(new Bundle {
    val rport = new Memory.ReadPort(addrWidth, dataBytes)
    val rwport = new Memory.ReadWritePort(addrWidth, dataBytes)
  })

  val mem = SyncReadMem(depth, Vec(dataBytes, UInt(8.W)))

  if (memoryFile != "") {
    loadMemoryFromFile(mem, memoryFile)
  }

  io.rport.data := mem.read(io.rport.addr, io.rport.en)

  io.rwport.rdata := mem.read(io.rwport.addr, io.rwport.en && !io.rwport.rw)
  when(io.rwport.en && io.rwport.rw) {
    mem.write(io.rwport.addr, io.rwport.wdata, io.rwport.wmask)
  }
}
