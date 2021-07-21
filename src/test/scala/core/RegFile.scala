package rvcore

import org.scalatest._
import chiseltest._
import chisel3._

class RegFileTest extends FreeSpec with ChiselScalatestTester {
  "Registers can keep values" in {
    test(new RegFile()(new RV64ICoreConfig)) { c =>
      def readExpect(addr: Int, value: Int, port: Int = 0): Unit = {
        c.io.rport.raddr(port).poke(addr.U)
        c.io.rport.rdata(port).expect(value.U)
      }
      def write(addr: Int, value: Int): Unit = {
        c.io.wb.rd.poke(addr.U)
        c.io.wb.data.poke(value.U)
        c.clock.step()
        c.io.wb.rd.poke(0.U)
      }

      // everything should be 0 on init
      for (i <- 0 until 32) {
        readExpect(i, 0, port = 0)
        readExpect(i, 0, port = 1)
      }

      // write 5 * addr + 3
      for (i <- 0 until 32)
        write(i, 5 * i + 3)

      // check that the writes worked
      readExpect(0, 0)
      for (i <- 1 until 32)
        readExpect(i, 5 * i + 3, port = i % 2)
    }
  }
}
