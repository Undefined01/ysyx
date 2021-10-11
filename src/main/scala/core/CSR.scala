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
    out.sd := out.fs =/= 0.U || out.xs =/= 0.U
    io.value := out.asUInt
    when(io.wen) {
      reg := io.wdata
    }
  }
  class IEBundle extends Bundle {
    val wpri_12 = UInt(52.W)
    val meie = Bool()
    val wpri_10 = Bool()
    val seie = Bool()
    val ueie = Bool()
    val mtie = Bool()
    val wpri_6 = Bool()
    val stie = Bool()
    val utie = Bool()
    val msie = Bool()
    val wpri_2 = Bool()
    val ssie = Bool()
    val usie = Bool()
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
      val is_timerintr = Input(Bool())
      val pc = Input(UInt(c.XLEN.W))

      val intr = Output(UInt(32.W))
      val jump = Output(new Bundle {
        val valid = Bool()
        val pc = UInt(c.XLEN.W)
      })
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
  val mstatusB = WireInit(mstatus.io.value.asTypeOf(new Csr.StatusBundle))
  val mie = Module(Csr(CsrNumber.mie))
  val mieB = WireInit(mie.io.value.asTypeOf(new Csr.IEBundle))
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

  io.trap.intr := 0.U
  io.trap.jump.valid := false.B
  io.trap.jump.pc := DontCare
  val intr = Wire(new Bundle {
    val valid = Bool()
    val cause = UInt(64.W)
  })
  intr.valid := false.B
  intr.cause := DontCare
  when(io.trap.is_timerintr && mstatusB.mie && mieB.mtie) {
    intr.valid := true.B
    intr.cause := "h80000000_00000007".U
    io.trap.intr := 7.U
  }.elsewhen(io.trap.is_ecall) {
    intr.valid := true.B
    intr.cause := 11.U
  }
  when(intr.valid) {
    mepc.io.wen := true.B
    mepc.io.wdata := io.trap.pc
    val mstatusNew = WireInit(mstatusB)
    mstatusNew.mpie := mstatusB.mie
    mstatusNew.mie := false.B
    mstatusNew.mpp := 3.U
    mstatus.io.wen := true.B
    mstatus.io.wdata := mstatusNew.asUInt
    mcause.io.wen := true.B
    mcause.io.wdata := intr.cause
    io.trap.jump.valid := true.B
    io.trap.jump.pc := mtvec.io.value
  }
  when(io.trap.is_mret) {
    val mstatusNew = WireInit(mstatusB)
    mstatusNew.mie := mstatusB.mpie
    mstatusNew.mpie := true.B
    mstatusNew.mpp := 0.U
    mstatus.io.wen := true.B
    mstatus.io.wdata := mstatusNew.asUInt
    io.trap.jump.valid := true.B
    io.trap.jump.pc := mepc.io.value
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
