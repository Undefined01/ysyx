package core

import chisel3._
import chisel3.tester._
import org.scalatest._

import firrtl.FileUtils
import java.io._

class CoreTest extends FreeSpec with ChiselScalatestTester {
  FileUtils.makeDirectory("test_run_dir/temp")
  def runTestCase(testcaseName: String) {
    MemoryTest.prepareMemoryFile(
      s"/basic/build/${testcaseName}.bin",
      s"test_run_dir/temp/${testcaseName}.hex",
      8
    )
    test(new Core(new RV64ICoreConfig {
      override val DebugPin = true
      override val MemoryFile = s"test_run_dir/temp/${testcaseName}.hex"
    })) { c =>
      println(s"Start running testcase `${testcaseName}`")
      var i = 0
      var trapped = false
      while (i <= 10000 && !trapped) {
        c.clock.step()
        i += 1

        val if_pc = c.io.debug.get.if_pc.peek().litValue
        val if_instr = c.io.debug.get.if_instr.peek().litValue
        if (if_instr == 0x7f2a214b) {
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
              val l = c.io.debug.get.reg(idx).peek().litValue
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
  "core can run basic immediate arithmetic instructions" in {
    // runTestCase("addi_with_nop")
    // runTestCase("addi")
    runTestCase("opimm")
  }
}
