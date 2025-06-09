package FPApprox.Primitives

import spinal.core._
import spinal.core.sim._
import FPApprox.Config
import scala.language.postfixOps


// MARK: LUT6_2 BlackBox
case class LUT6_2_prim(INIT: BigInt) extends BlackBox {

  addGeneric("INIT", B(INIT, 64 bits))

  val io = new Bundle {
    val I0 = in  Bool()
    val I1 = in  Bool()
    val I2 = in  Bool()
    val I3 = in  Bool()
    val I4 = in  Bool()
    val I5 = in  Bool()

    val O5 = out Bool()
    val O6 = out Bool()
  }
  noIoPrefix()

  setDefinitionName("LUT6_2")

}


// MARK: Top of LUT6_2 primitives
case class LUT6_2_Hard(INIT: BigInt) extends Component {
  val io = new Bundle {
    val I  = in  Bits(6 bits)
    val O5 = out Bool()
    val O6 = out Bool()
  }
  noIoPrefix()

  val LUT6_2_inst = new LUT6_2_prim(INIT)
  LUT6_2_inst.io.I0 := io.I(0)
  LUT6_2_inst.io.I1 := io.I(1)
  LUT6_2_inst.io.I2 := io.I(2)
  LUT6_2_inst.io.I3 := io.I(3)
  LUT6_2_inst.io.I4 := io.I(4)
  LUT6_2_inst.io.I5 := io.I(5)

  io.O5 := LUT6_2_inst.io.O5
  io.O6 := LUT6_2_inst.io.O6

}


object LUT6_2_Hard_RTL extends App {
  Config.setGenSubDir("/HBU")
  Config.spinal.generateVerilog(LUT6_2_Hard(INIT=BigInt("1A2B3C4D1A2B3C4D", 16))).printRtl()
}