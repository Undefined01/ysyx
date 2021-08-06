package rvcore

import org.scalatest._
import chiseltest._
import chisel3._
import chisel3.util.experimental.BoringUtils

import firrtl.FileUtils
import java.io._

import device._

class CoreTest extends FreeSpec with ChiselScalatestTester {
  class ScalaTestTop(c: CoreConfig, memoryFile: String)
      extends Module {
    class DiffTestIO extends Bundle {
      val reg = Output(Vec(32, UInt(c.XLEN.W)))
      val if_pc = Output(UInt(c.XLEN.W))
      val if_instr = Output(UInt(c.InstrLen.W))
    }

    val io = IO(new DiffTestIO)

    val ram = Module(
      new RAM(
        addrWidth = c.XLEN,
        dataBytes = c.XLEN / 8,
        depth = 40 * 1024 / c.XLEN,
        memoryFile = memoryFile
      )
    )
    val core = Module(new RvCore()(c))
    core.io.ram <> ram.io

    io := DontCare
    io.reg := core.io.debug.get.reg
    
    // BoringUtils.addSink(io.reg, "RegFile_regs")
    BoringUtils.addSink(io.if_pc, "IF_pc")
    BoringUtils.addSink(io.if_instr, "IF_instr")
  }

  FileUtils.makeDirectory("test_run_dir/temp")
  def runTestCase(testcaseName: String) {
    RamTest.prepareMemoryFile(
      s"/basic/build/${testcaseName}.bin",
      s"test_run_dir/temp/${testcaseName}.hex",
      8
    )
    test(
      new ScalaTestTop(
        new RV64ICoreConfig {
          override val InitialPC = BigInt("0")
          override val DebugPort = true
        },
        memoryFile = s"test_run_dir/temp/${testcaseName}.hex"
      )
    ) { c =>
      println(s"Start running testcase `${testcaseName}`")
      var i = 0
      var trapped = false
      while (i <= 10000 && !trapped) {
        c.clock.step()
        i += 1

        val if_pc = c.io.if_pc.peek().litValue
        val if_instr = c.io.if_instr.peek().litValue
        if (if_instr == 0x0000006b) {
          trapped = true
          println(f"Hit good trap at 0x${if_pc}%x after ${i}%d steps")

          val f =
            getClass.getResourceAsStream(s"/basic/expect/${testcaseName}.txt")
          if (f == null) {
            throw new RuntimeException(
              s"Unable to read `/basic/expect/${testcaseName}.txt`"
            )
          }
          val reader = new BufferedReader(new InputStreamReader(f))
          Iterator
            .continually(reader.readLine())
            .takeWhile(_ != null)
            .zipWithIndex
            .foreach { case (x, idx) =>
              val l = c.io.reg(idx).peek().litValue
              val r = BigInt(x, 16)
              if (l != r) {
                println(f"ERROR Reg ${idx}%d got 0x${l}%x expect 0x${r}%x\n")
                throw new RuntimeException(s"Assertion fail")
              }
            }
          println(s"PASS test case `${testcaseName}`")
        }
      }
    }
  }
  // "core can execute integer computational instructions" in {
  //   runTestCase("addi_with_nop")
  //   runTestCase("addi")
  //   runTestCase("opimm")
  //   runTestCase("op")
  //   runTestCase("u_instr")
  // }
  // "core can execute control transfer instructions" in {
  //   runTestCase("jump")
  //   runTestCase("branch")
  // }
  // "core can execute load and store instructions" in {
  //   runTestCase("ldst")
  // }
  // "core can execute 32-bit integer computational instructions" in {
  //   runTestCase("op32")
  // }
  "core can read mcycle CSR" in {
    runTestCase("csr_mcycle")
  }
}
