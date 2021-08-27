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
  val sstatus = 0x100

  val mstatus = 0x300
  val mie = 0x304
  val mtvec = 0x305

  val mscratch = 0x340
  val mepc = 0x341
  val mcause = 0x342
  val mtval = 0x343
  val mip = 0x344

  val mcycle = 0xb00
}

object Csr {
  class Status {
    val fs = RegInit(0.U(2.W))
    val xs = RegInit(0.U(2.W))
    val sd = fs.orR || xs.orR
  }
  abstract class CsrModule extends Module {
    val io = IO(new Bundle {
      val value = Output(UInt(64.W))
      val wen = Input(Bool())
      val wdata = Input(UInt(64.W))
    })

    val number: Int
    val skip: Boolean = false

    val reg = RegInit(0.U(64.W))
    val wpri_mask: BigInt = BigInt("ffffffffffffffff", 16)
    val wpri = reg & wpri_mask.U
    io.value := wpri
  }
  def apply(csrNumber: Int) = new CsrModule {
    val number = csrNumber

    when(io.wen) {
      reg := io.wdata
    }
  }
  class Mcycle extends CsrModule {
    val number = CsrNumber.mcycle
    override val skip = true
    reg := reg + 1.U
  }
  class StatusBundle extends Bundle {
    val sd = Bool()
    val dontcare = UInt(46.W)
    val xs = UInt(2.W)
    val fs = UInt(2.W)
    val mpp = UInt(2.W)
    val wpri_10_9 = UInt(2.W)
    val spp = Bool()
    val mpie = Bool()
    val wpri_6 = Bool()
    val spie = Bool()
    val upie = Bool()
    val mie = Bool()
    val wpri_2 = Bool()
    val sie = Bool()
    val uie = Bool()
  }
  class MStatus extends CsrModule {
    val number = CsrNumber.mstatus

    val out = WireInit(wpri.asTypeOf(new StatusBundle))
    out.sd := out.fs.orR || out.xs.orR
    io.value := out.asUInt
    when(io.wen) {
      reg := io.wdata
    }
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

    val trap = new Bundle {
      val is_ecall = Input(Bool())
      val is_mret = Input(Bool())
      val pc = Input(UInt(c.XLEN.W))
      val jump_pc = Output(UInt(c.XLEN.W))
    }
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

  val mstatus = Module(new Csr.MStatus)
  val mie = Module(Csr(CsrNumber.mie))
  val mtvec = Module(Csr(CsrNumber.mtvec))
  val mscratch = Module(Csr(CsrNumber.mscratch))
  val mepc = Module(Csr(CsrNumber.mepc))
  val mcause = Module(Csr(CsrNumber.mcause))

  val mcycle = Module(new Csr.Mcycle)

  val csrs: List[Csr.CsrModule] =
    List(mstatus, mie, mtvec, mscratch, mepc, mcause, mcycle)

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

  io.trap.jump_pc := DontCare
  when(io.trap.is_ecall) {
    mepc.io.wen := true.B
    mepc.io.wdata := io.trap.pc
    val mstatusOld = WireInit(mstatus.io.value.asTypeOf(new Csr.StatusBundle))
    val mstatusNew = WireInit(mstatus.io.value.asTypeOf(new Csr.StatusBundle))
    mstatusNew.mpie := mstatusOld.mie
    mstatusNew.mie := false.B
    mstatusNew.mpp := 3.U
    mstatus.io.wen := true.B
    mstatus.io.wdata := mstatusNew.asUInt
    mcause.io.wen := true.B
    mcause.io.wdata := 11.U
    io.trap.jump_pc := mtvec.io.value
  }
  when(io.trap.is_mret) {
    val mstatusOld = WireInit(mstatus.io.value.asTypeOf(new Csr.StatusBundle))
    val mstatusNew = WireInit(mstatus.io.value.asTypeOf(new Csr.StatusBundle))
    mstatusNew.mie := mstatusOld.mpie
    mstatusNew.mpie := true.B
    mstatusNew.mpp := 0.U
    mstatus.io.wen := true.B
    mstatus.io.wdata := mstatusNew.asUInt
    io.trap.jump_pc := mepc.io.value
  }

  if (c.DiffTest) {
    val csr = Module(new difftest.DifftestCSRState)
    csr.io.clock := clock
    csr.io.coreid := c.CoreId.U
    csr.io.mstatus := mstatus.io.value
    csr.io.mcause := mcause.io.value
    csr.io.mepc := mepc.io.value
    csr.io.sstatus := mstatus.io.value & "h80000003000DE122".U
    csr.io.scause := 0.U
    csr.io.sepc := 0.U
    csr.io.satp := 0.U
    csr.io.mip := 0.U
    csr.io.mie := mie.io.value
    csr.io.mscratch := mscratch.io.value
    csr.io.sscratch := 0.U
    csr.io.mideleg := 0.U
    csr.io.medeleg := 0.U
    csr.io.mtval := 0.U
    csr.io.stval := 0.U
    csr.io.mtvec := mtvec.io.value
    csr.io.stvec := 0.U
    csr.io.priviledgeMode := 3.U
  }
}
