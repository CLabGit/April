package FPApprox.Primitives

import spinal.core._
import spinal.core.sim._
import FPApprox.Config
import scala.language.postfixOps


case class LUT6_Soft(INIT: BigInt) extends LUT6 {

  val INITBinString = String.format("%64s", INIT.toString(2)).replace(" ", "0")    // Must be 64 bits width
//  println(INITBinString)

  switch(io.I) {
    for (i <- 0 until 64) {
      is(B(i, 6 bits)) {
        val ithBitChar = INITBinString.reverse.charAt(i).toString    // Need to reverse because it starts from LSB
        val ithBitBool = if (ithBitChar == "1") true else false
        io.O := Bool(ithBitBool)
      }
    }
  }

}


object LUT6_Soft_RTL extends App {
  Config.setGenSubDir("/LUT6")
//  Config.spinal.generateVerilog(LUT6_Soft(INIT=BigInt("1", 16))).printRtl()
  Config.spinal.generateVerilog(LUT6_Soft(INIT=BigInt("1A2B3C4D1A2B3C4D", 16))).printRtl()
}