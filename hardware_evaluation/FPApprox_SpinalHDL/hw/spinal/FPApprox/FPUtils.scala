package FPApprox

import spinal.core._
import spinal.core.sim._
import FPApprox.Config
import scala.language.postfixOps
import scala.math


// * Floating Point Formats
class FP[This <: FP[This]](ExpoWidth: Int, MantWidth: Int) extends Bundle {

  self: This =>

  val Sign = Bool()
  val Expo = UInt(ExpoWidth bits)    // Exponent
  val Mant = UInt(MantWidth bits)    // Mantissa

  def toBits: Bits = {
    Sign.asBits ## Expo.asBits ## Mant.asBits
  }

  def setValue(SignValue: Bool, ExpoValue: UInt, MantValue: UInt): Unit = {
    Sign := SignValue
    Expo := ExpoValue
    Mant := MantValue
  }

  def toReg(initSign: Bool, initExpo: UInt, initMant: UInt): This = {
    val FP_Reg = Reg(cloneOf(this)).asInstanceOf[This]
    FP_Reg.Sign.init(initSign)
    FP_Reg.Expo.init(initExpo)
    FP_Reg.Mant.init(initMant)
    FP_Reg
  }

}



// * FP formats with any ExpoWidth & any MantWidth
case class FP_Any(ExpoWidth: Int, MantWidth: Int) extends FP[FP_Any](ExpoWidth, MantWidth)

// * Some often used formats
case class FP8_E2M5() extends FP[FP8_E2M5](ExpoWidth=2, MantWidth=5)

case class FP8_E4M3() extends FP[FP8_E4M3](ExpoWidth=4, MantWidth=3)

case class FP8_E5M2() extends FP[FP8_E5M2](ExpoWidth=5, MantWidth=2)



class FPAnySimTools() {

  def ViewIntAsFPAny(Din: Int, ExpoWidth: Int, MantWidth: Int): (Boolean, Int, Int) = {
    val Sign = ((Din >> (ExpoWidth+MantWidth)) & 1) == 1
    val Expo = (Din >> MantWidth) & ((1 << ExpoWidth) - 1)
    val Mant = Din & ((1 << MantWidth) - 1)
    (Sign, Expo, Mant)
  }

}



class FP8Utils {

  def BitsToFP8_E4M3(DinBits: Bits) = {
    assert(DinBits.getBitsWidth == 8)

    val AsFP8_E4M3 = FP8_E4M3()
    AsFP8_E4M3.Sign := DinBits(7)
    AsFP8_E4M3.Expo := DinBits(6 downto 3).asUInt
    AsFP8_E4M3.Mant := DinBits(2 downto 0).asUInt

    AsFP8_E4M3        // return
  }


}
