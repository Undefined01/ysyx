package core

import firrtl.FileUtils
import java.io._
import java.nio.file.{Files, Paths}
import java.nio.file.StandardCopyOption.REPLACE_EXISTING

import chisel3._
import chisel3.tester._
import org.scalatest.FreeSpec
import chisel3.util.experimental.loadMemoryFromFile

object MemoryTest {
  def prepareMemoryFile(hexfile: String, target_path: String, bytes: Int) = {
    val (basename, extname) = {
      val tmp = target_path.split("\\.(?=[^\\.]+$)")
      (tmp(0), tmp(1))
    }

    val src = getClass.getResourceAsStream(hexfile)
    if (src == null) {
      throw new RuntimeException(s"Unable to get resource `$hexfile`")
    }
    val bsrc = new BufferedInputStream(src)

    val target = (0 until bytes) map { x =>
      val filename = s"${basename}_$x.$extname"
      val res = new FileOutputStream(filename)
      if (res == null)
        throw new RuntimeException(s"Unable to create memoryfile `$filename`")
      res
    }

    var idx = 0
    Iterator.continually(bsrc.read()).takeWhile(_ != -1).foreach { ch =>
      target(idx).write(f"${ch.toByte}%02X\n".getBytes())
      idx += 1
      if (idx == bytes)
        idx = 0
    }
  }
}

class MemoryTest extends FreeSpec with ChiselScalatestTester {
  "Read and write with mask" in {
    FileUtils.makeDirectory("test_run_dir/temp")
    MemoryTest.prepareMemoryFile("/mem1.txt", "test_run_dir/temp/mem1.txt", 4)
    test(
      new Memory(
        addrWidth = 8,
        dataBytes = 4,
        depth = 4,
        memoryFile = "test_run_dir/temp/mem1.txt"
      )
    ) { c =>
      def readExpect(addr: Int, value: Seq[Int]): Unit = {
        c.io.rport.en.poke(true.B)
        c.io.rport.addr.poke(addr.U)
        c.clock.step()
        value.zipWithIndex.foreach { case (v, idx) =>
          c.io.rport.data(idx).expect(v.U)
        }
        c.io.rport.en.poke(false.B)
      }
      def write(addr: Int, mask: Seq[Boolean], value: Seq[Int]): Unit = {
        c.io.rwport.en.poke(true.B)
        c.io.rwport.rw.poke(true.B)
        c.io.rwport.addr.poke(addr.U)
        mask.zipWithIndex.foreach { case (v, idx) =>
          c.io.rwport.wmask(idx).poke(v.B)
        }
        value.zipWithIndex.foreach { case (v, idx) =>
          c.io.rwport.wdata(idx).poke(v.U)
        }
        c.clock.step()
        c.io.rwport.en.poke(false.B)
      }

      // Memory should be initialized by memory file
      for (addr <- 0 until 4) {
        readExpect(
          addr,
          (0 until 4).map { x => 'a'.toByte + addr * 4 + x }
        )
      }

      // Changes can be read after writing
      for (addr <- 0 until 4) {
        write(addr, Seq.fill(4)(true), Seq.fill(4)(0xcd))
        readExpect(addr, Seq.fill(4)(0xcd))
      }

      // write mask should take effect
      write(
        0,
        Seq(true, false, true, false),
        Seq(0x12, 0x34, 0x56, 0x78)
      )
      readExpect(0, Seq(0x12, 0xcd, 0x56, 0xcd))
    }
  }
}
