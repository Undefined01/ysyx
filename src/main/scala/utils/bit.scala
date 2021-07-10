package utils

import chisel3._
import chisel3.util._

object ZeroExt {
  def apply(x: UInt, len: Int) = {
    require(x.getWidth > 0)
    if (x.getWidth >= len)
      x(len - 1, 0)
    else
      Cat(0.U((len - x.getWidth).W), x)
  }
}

object SignExt {
  def apply(x: UInt, len: Int) = {
    require(x.getWidth > 0)
    if (x.getWidth >= len)
      x(len - 1, 0)
    else
      Cat(Fill(len - x.getWidth, x(x.getWidth - 1)), x)
  }
}
