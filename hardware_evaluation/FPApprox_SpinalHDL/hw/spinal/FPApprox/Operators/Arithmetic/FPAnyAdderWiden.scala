package FPApprox.Operators.Arithmetic

import spinal.core._
import spinal.core.sim._
import FPApprox.Config
import scala.language.postfixOps
import FPApprox.FP_Any


// MARK: BlackBox for fpany_adder_widen.v file
case class fpany_adder_widen(ExpoWidthMult: Int, MantWidthMult: Int, ExpoWidthPSum: Int, MantWidthPSum: Int) extends BlackBox {

  // Generics
  addGeneric("EXPO_WIDTH_MULT", ExpoWidthMult)
  addGeneric("MANT_WIDTH_MULT", MantWidthMult)
  addGeneric("EXPO_WIDTH_PSUM", ExpoWidthPSum)
  addGeneric("MANT_WIDTH_PSUM", MantWidthPSum)

  val io = new Bundle {
    val a = in  Bits(1+ExpoWidthMult+MantWidthMult bits)
    val b = in  Bits(1+ExpoWidthPSum+MantWidthPSum bits)
    val r = out Bits(1+ExpoWidthPSum+MantWidthPSum bits)
  }
  noIoPrefix()

  // ? Be careful to the blackbox import path
  addRTLPath(s"hw/spinal/FPApprox/BlackBoxImport/fpany_adder_widen.v")

}



// MARK: FP8Any FloatingPoint Adder
case class FPAnyAdderWiden(ExpoWidthMult: Int, MantWidthMult: Int, ExpoWidthPSum: Int, MantWidthPSum: Int, withFF: Boolean=false) extends Component {
  val io = new Bundle {
    val A   = in  Bits(1+ExpoWidthMult+MantWidthMult bits)
    val B   = in  Bits(1+ExpoWidthPSum+MantWidthPSum bits)
    val Sum = out Bits(1+ExpoWidthPSum+MantWidthPSum bits)
  }
  noIoPrefix()

  val FPAnyAdd = new fpany_adder_widen(ExpoWidthMult=ExpoWidthMult, MantWidthMult=MantWidthMult, ExpoWidthPSum=ExpoWidthPSum, MantWidthPSum=MantWidthPSum)

  FPAnyAdd.io.a := io.A
  FPAnyAdd.io.b := io.B

  val ResultFinal = if (withFF) { RegNext(FPAnyAdd.io.r).init(B(0)) } else { FPAnyAdd.io.r }

  io.Sum := ResultFinal

}



object FFPAnyAdderWidenGenRTL extends App {
  Config.setGenSubDir("/Arithmetic")
  Config.spinal.generateVerilog(
    FPAnyAdderWiden(ExpoWidthMult=4, MantWidthMult=3, ExpoWidthPSum=7, MantWidthPSum=4, withFF=true)
  ).printRtl().mergeRTLSource()
}