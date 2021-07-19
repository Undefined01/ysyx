package device

import org.scalatest._
import chiseltest._
import chisel3._
import chisel3.util.experimental.loadMemoryFromFile

import firrtl.FileUtils
import java.io._
import java.nio.file.{Files, Paths}
import java.nio.file.StandardCopyOption.REPLACE_EXISTING

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
  // "Read and write with mask" in {
  //   FileUtils.makeDirectory("test_run_dir/temp")
  //   MemoryTest.prepareMemoryFile("/mem1.txt", "test_run_dir/temp/mem1.txt", 4)
  //   test(
  //     new Memory(
  //       addrWidth = 8,
  //       dataBytes = 4,
  //       depth = 4,
  //       memoryFile = "test_run_dir/temp/mem1.txt"
  //     )
  //   ) { c =>
  //     def readExpect(addr: Int, value: Seq[Int]): Unit = {
  //       c.io.en.poke(true.B)
  //       c.io.rport.addr.poke(addr.U)
  //       c.clock.step()
  //       value.zipWithIndex.foreach { case (v, idx) =>
  //         c.io.rport.data(idx).expect(v.U)
  //       }
  //       c.io.rport.en.poke(false.B)
  //     }
  //     def write(addr: Int, mask: Seq[Boolean], value: Seq[Int]): Unit = {
  //       c.io.rwport.en.poke(true.B)
  //       c.io.rwport.rw.poke(true.B)
  //       c.io.rwport.addr.poke(addr.U)
  //       mask.zipWithIndex.foreach { case (v, idx) =>
  //         c.io.rwport.wmask(idx).poke(v.B)
  //       }
  //       value.zipWithIndex.foreach { case (v, idx) =>
  //         c.io.rwport.wdata(idx).poke(v.U)
  //       }
  //       c.clock.step()
  //       c.io.rwport.en.poke(false.B)
  //     }

  //     // Memory should be initialized by memory file
  //     for (addr <- 0 until 4) {
  //       readExpect(
  //         addr,
  //         (0 until 4).map { x => 'a'.toByte + addr * 4 + x }
  //       )
  //     }

  //     // Changes can be read after writing
  //     for (addr <- 0 until 4) {
  //       write(addr, Seq.fill(4)(true), Seq.fill(4)(0xcd))
  //       readExpect(addr, Seq.fill(4)(0xcd))
  //     }

  //     // write mask should take effect
  //     write(
  //       0,
  //       Seq(true, false, true, false),
  //       Seq(0x12, 0x34, 0x56, 0x78)
  //     )
  //     readExpect(0, Seq(0x12, 0xcd, 0x56, 0xcd))
  //   }
  // }

  "Driver should produce correct values" in {
    test(new RAM.PortDriver(32, 8)) { c =>
      def testRead(
          addr: Int,
          unsigned: Boolean,
          width: Int,
          data: BigInt
      ) = {
        c.io.raddr.poke(addr.U)
        c.io.rUnsigned.poke(unsigned.B)
        c.io.rWidth.poke(width.U)

        c.io.ram_ctrl.raddr.expect((addr >> 3).U)
        c.clock.step()
        c.io.ram_ctrl.rdata.poke(data.U)

        val sub_addr = addr & 7
        var expect = BigInt(0)
        for (i <- 0 until 1 << width)
          expect |= ((data >> (i + sub_addr) * 8) & 0xff) << (i * 8)
        val signBit = expect & 1 << ((1 << width) * 8 - 1)
        if (!unsigned && signBit != 0)
          for (i <- 1 << width until 8)
            expect |= BigInt(0xff) << (i * 8)
        c.io.rdata.expect(expect.U)
      }
      def testWrite(
          addr: Int,
          width: Int,
          data: BigInt
      ) = {
        c.io.waddr.poke(addr.U)
        c.io.wWidth.poke(width.U)
        c.io.wdata.poke(data.U)
        c.clock.step()

        val sub_addr = addr & 7
        var wmask = c.io.ram_ctrl.wmask.peek().litValue
        var wdata = c.io.ram_ctrl.wdata.peek().litValue
        for (i <- 0 until 8) {
          val mask = sub_addr <= i && i < sub_addr + (1 << width)
          assert((wmask & 0xff) == (if (mask) 0xff else 0))
          if (mask)
            assert((wdata & 0xff) == ((data >> ((i - sub_addr) * 8)) & 0xff))
          wmask >>= 8
          wdata >>= 8
        }
      }

      testRead(0, false, 0, BigInt("deedbeeffeedbabe", 16))
      testRead(3, false, 0, BigInt("deedbeeffeedbabe", 16))
      testRead(7, false, 0, BigInt("deedbeeffeedbabe", 16))
      testRead(6, false, 1, BigInt("deedbeeffeedbabe", 16))
      testRead(4, false, 2, BigInt("deedbeeffeedbabe", 16))
      testRead(0, false, 3, BigInt("deedbeeffeedbabe", 16))

      testRead(0, true, 0, BigInt("deedbeeffeedbabe", 16))
      testRead(3, true, 0, BigInt("deedbeeffeedbabe", 16))
      testRead(7, true, 0, BigInt("deedbeeffeedbabe", 16))
      testRead(6, true, 1, BigInt("deedbeeffeedbabe", 16))
      testRead(4, true, 2, BigInt("deedbeeffeedbabe", 16))
      testRead(0, true, 3, BigInt("deedbeeffeedbabe", 16))

      testWrite(0, 0, BigInt("123456789abcdef9", 16))
      testWrite(3, 0, BigInt("123456789abcdef9", 16))
      testWrite(7, 0, BigInt("123456789abcdef9", 16))
      testWrite(6, 1, BigInt("123456789abcdef9", 16))
      testWrite(4, 2, BigInt("123456789abcdef9", 16))
      testWrite(0, 3, BigInt("123456789abcdef9", 16))
    }
  }
}
