package utils

import chisel3._
import chisel3.util._

import java.awt.print.Printable

object Logger {
  // val LogFilter = LogLevel.WARN
  val LogFilter = LogLevel.DEBUG

  object LogLevel extends Enumeration {
    type LogLevel = Value
    val DEBUG, INFO, WARN, ERROR, None = Value
  }

  class Logger(val logLevel: LogLevel.LogLevel) {
    def apply(cond: Bool, fmt: String, data: Bits*): Unit = {
      if (logLevel >= LogFilter) {
        when(cond) {
          printf(fmt, data: _*)
        }
      }
    }
    def apply(fmt: String, data: Bits*): Unit = apply(true.B, fmt, data: _*)
  }

  object Debug extends Logger(LogLevel.DEBUG)
  object Info extends Logger(LogLevel.INFO)
  object Warn extends Logger(LogLevel.WARN)
  object Error extends Logger(LogLevel.ERROR)
}
