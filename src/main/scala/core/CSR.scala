package rvcore

import chisel3._
import chisel3.util._
import utils._

object CsrFn {
  val WR = 1
  val SET = 2
  val CLR = 3

  val bits = 2
}

object CsrNumber {
  val mcycle = 0xb00
}

object Csr {
  abstract class CsrModule extends Module {
    val io = IO(new Bundle {
      val value = Output(UInt(64.W))
      val wen = Input(Bool())
      val wdata = Input(UInt(64.W))
    })

    val reg = RegInit(0.U(64.W))
    io.value := reg

    val number: Int
    val skip: Boolean
  }
  class Mcycle extends CsrModule {
    val number = CsrNumber.mcycle
    val skip = true
    reg := reg + 1.U
  }
}

class Csr(implicit c: CoreConfig) extends Module {
  val io = IO(new Bundle {
    val en = Input(Bool())
    val csrfn = Input(UInt(CsrFn.bits.W))
    val csr_number = Input(UInt(12.W))
    val op = Input(UInt(c.XLEN.W))
    val res = Output(UInt(c.XLEN.W))
    val skip = Output(Bool())
  })

  val csr_value = Wire(UInt(64.W))
  val res = MuxLookup(
    io.csrfn,
    0.U(64.W),
    List(
      CsrFn.WR.U -> io.op,
      CsrFn.SET.U -> (csr_value | io.op),
      CsrFn.CLR.U -> (csr_value & ~io.op)
    )
  )

  val mcycle = Module(new Csr.Mcycle)

  val csrs: List[Csr.CsrModule] = List(mcycle)
  csr_value := MuxLookup(
    io.csr_number,
    0.U(64.W),
    csrs.map { x => x.number.U -> x.io.value }
  )
  csrs.foreach { x =>
    x.io.wen := io.en && io.csr_number === x.number.U
    x.io.wdata := res
  }

  io.res := csr_value
  io.skip := MuxLookup(
    io.csr_number,
    0.U(64.W),
    csrs.map { x => x.number.U -> x.skip.B }
  )
}
