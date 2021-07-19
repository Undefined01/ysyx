package device

import chisel3._
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFile

import utils._

object RAM {
  class PortDriver(addrWidth: Int, dataBytes: Int) extends Module {
    require(isPow2(dataBytes))
    val io = IO(new Bundle {
      val raddr = Input(UInt(addrWidth.W))
      val rUnsigned = Input(Bool())
      val rWidth = Input(UInt(log2Ceil(log2Ceil(dataBytes) + 1).W))
      val waddr = Input(UInt(addrWidth.W))
      val wWidth = Input(UInt(log2Ceil(log2Ceil(dataBytes) + 1).W))
      val wdata = Input(UInt((dataBytes * 8).W))
      val rdata = Output(UInt((dataBytes * 8).W))

      val ram_ctrl = new Bundle {
        val raddr = Output(UInt(addrWidth.W))
        val rdata = Input(UInt((dataBytes * 8).W))
        val waddr = Output(UInt(addrWidth.W))
        val wmask = Output(UInt((dataBytes * 8).W))
        val wdata = Output(UInt((dataBytes * 8).W))
      }
    })

    val masks = VecInit((0 to log2Ceil(dataBytes)).map { x =>
      Cat((0 until dataBytes).reverse.map { y =>
        Fill(8, (y < (1 << x)).B.asUInt)
      })
    })

    val rUnsigned = RegNext(io.rUnsigned)
    val rWidth = RegNext(io.rWidth)
    val rSubaddr = RegNext(io.raddr(log2Ceil(dataBytes) - 1, 0))

    io.ram_ctrl.raddr := ZeroExt(
      io.raddr(addrWidth - 1, log2Ceil(dataBytes)),
      addrWidth
    )
    val rdata = (io.ram_ctrl.rdata >> (rSubaddr * 8.U)) & masks(rWidth)
    io.rdata := Mux(
      rUnsigned,
      rdata,
      VecInit((0 to log2Ceil(dataBytes)).map { x =>
        SignExt(rdata((1 << x) * 8 - 1, 0), dataBytes * 8)
      })(rWidth)
    )

    val wSubaddr = io.waddr(log2Ceil(dataBytes) - 1, 0)
    io.ram_ctrl.waddr := ZeroExt(
      io.waddr(addrWidth - 1, log2Ceil(dataBytes)),
      addrWidth
    )
    io.ram_ctrl.wmask := masks(io.wWidth) << (wSubaddr * 8.U)
    io.ram_ctrl.wdata := io.wdata << (wSubaddr * 8.U)
  }
}

class RAM(
    addrWidth: Int,
    dataBytes: Int,
    depth: Int,
    memoryFile: String = ""
) extends Module {
  val io = IO(new Bundle {
    val en = Input(Bool())
    val raddr = Input(UInt(addrWidth.W))
    val rdata = Output(UInt((dataBytes * 8).W))
    val wen = Input(Bool())
    val waddr = Input(UInt(addrWidth.W))
    val wmask = Input(UInt((dataBytes * 8).W))
    val wdata = Input(UInt((dataBytes * 8).W))
  })

  val mem = SyncReadMem(depth, Vec(dataBytes, UInt(8.W)))

  if (memoryFile != "") {
    loadMemoryFromFile(mem, memoryFile)
  }

  io.rdata := Cat(mem.read(io.raddr, io.en))

  val wdata = VecInit((0 until dataBytes).map { x =>
    io.wdata(x * 8 + 7, x * 8)
  })
  val wmask = VecInit((0 until dataBytes).map { x =>
    io.wmask(x * 8).asBool
  })
  when(io.en && io.wen) {
    mem.write(io.waddr, wdata, wmask)
  }
}
