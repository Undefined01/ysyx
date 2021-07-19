package rvcore

import chisel3._
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFile

import utils._

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
  class PortDriver(addrWidth: Int, dataBytes: Int) extends Module {
    require(isPow2(dataBytes))
    val io = IO(new Bundle {
      val in = new Bundle {
        val addr = Input(UInt(addrWidth.W))
        val unsigned = Input(Bool())
        val wWidth = Input(UInt(log2Ceil(log2Ceil(dataBytes) + 1).W))
        val wdata = Input(UInt((dataBytes * 8).W))
      }
      val mem_ctrl = new Bundle {
        val addr = Output(UInt(addrWidth.W))
        val rdata = Input(Vec(dataBytes, UInt(8.W)))
        val wmask = Output(Vec(dataBytes, Bool()))
        val wdata = Output(Vec(dataBytes, UInt(8.W)))
      }
      val out = new Bundle {
        val rdata = Output(UInt((dataBytes * 8).W))
      }
    })

    val unsigned = RegNext(io.in.unsigned)
    val rWidth = RegNext(io.in.wWidth)

    io.mem_ctrl.addr := ZeroExt(
      io.in.addr(addrWidth - 1, log2Ceil(dataBytes)),
      addrWidth
    )
    (0 until dataBytes).foreach { x =>
      io.mem_ctrl.wmask(x) := VecInit(
        (0 to log2Ceil(dataBytes))
          .map { y =>
            if (y == log2Ceil(dataBytes)) {
              (io.in.wWidth === y.U)
            } else {
              (io.in.wWidth === y.U) &&
              (io.in.addr(log2Ceil(dataBytes) - 1, y) === (x >> y).U)
            }
          }
      ).reduceTree(_ || _)
    }
    // little endian
    (0 until dataBytes).foreach { b =>
      io.mem_ctrl.wdata(b) := VecInit((0 until dataBytes).map { x =>
        if (b - x >= 0)
          io.in.wdata((b - x) * 8 + 7, (b - x) * 8)
        else
          0.U
      })(io.in.addr(log2Ceil(dataBytes) - 1, 0))
    }
    io.out.rdata := VecInit((0 to log2Ceil(dataBytes)).map { y =>
      if (y == log2Ceil(dataBytes)) {
        Cat(io.mem_ctrl.rdata.reverse)
      } else {
        VecInit((0 until 1 << (log2Ceil(dataBytes) - y)).map { x =>
          val data = Cat((0 until 1 << y).reverse.map { b =>
            io.mem_ctrl.rdata((x << y) + b)
          })
          Mux(
            unsigned,
            ZeroExt(data, dataBytes * 8),
            SignExt(data, dataBytes * 8)
          )
        })(io.in.addr(log2Ceil(dataBytes) - 1, y))
      }
    })(rWidth)
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
