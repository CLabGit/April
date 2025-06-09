package FPApprox.Operators.Arithmetic

import FPApprox.Config
import spinal.core._
import spinal.core.sim._

import scala.language.postfixOps



// MARK: BlackBox for ternary_adder_noincr.v file
case class ternary_adder_noincr() extends BlackBox {
  val io = new Bundle {
    val x   = in  Bits(8 bits)
    val y   = in  Bits(8 bits)
    val z   = in  Bits(8 bits)
    val cin = in  Bool()
    val sum = out Bits(8 bits)
  }
  noIoPrefix()

  // ? Be careful to the blackbox import path
  addRTLPath(s"hw/spinal/FPApprox/BlackBoxImport/ternary_adder_noincr.v")
}



// MARK: This TernaryAdder is only for FP8 Approximation
// MARK: TernaryAdderFP8_Hard & TernaryAdderFP8_Soft extend from this
class TernaryAdderFP8(withFF: Boolean) extends Component {
  val io = new Bundle {
    val X   = in  Bits(8 bits)
    val Y   = in  Bits(8 bits)
    val Z   = in  Bits(8 bits)
    val Cin = in  Bool()
    val Sum = out Bits(8 bits)
  }
  noIoPrefix()
}



// MARK: Using TernaryAdder Optimization (with Primitives)
// MARK: This TernaryAdder is only for FP8 Approximation
case class TernaryAdderFP8_Hard(withFF: Boolean=false) extends TernaryAdderFP8(withFF) {

  val TerAdd = new ternary_adder_noincr()

  TerAdd.io.x   := io.X
  TerAdd.io.y   := io.Y
  TerAdd.io.z   := io.Z
  TerAdd.io.cin := io.Cin

  val Result = withFF match {
    case true  => Reg(Bits(8 bits)).init(B(0))
    case false => Bits(8 bits)
  }

  Result := TerAdd.io.sum
  io.Sum := Result

}


object TernaryAdderFP8_Hard_GenRTL extends App {
  Config.setGenSubDir("/FP8")
  Config.spinal.generateVerilog(TernaryAdderFP8_Hard(withFF=true)).printRtl().mergeRTLSource()
}




// MARK: Without TernaryAdder Optimization (Pure adding)
// MARK: This TernaryAdder is only for FP8 Approximation
case class TernaryAdderFP8_Soft(withFF: Boolean=false) extends TernaryAdderFP8(withFF) {

  val Result = withFF match {
    case true  => Reg(Bits(8 bits)).init(B(0))
    case false => Bits(8 bits)
  }

  Result := (io.X.asSInt + io.Y.asSInt + io.Z.asSInt + io.Cin.asBits.resize(8).asSInt).asBits    // ! CHECKME
  io.Sum := Result

}


object TernaryAdderFP8_Soft_GenRTL extends App {
  Config.setGenSubDir("/FP8")
  Config.spinal.generateVerilog(TernaryAdderFP8_Soft(withFF=true)).printRtl()
}



object TernaryAdderFP8_Soft_Sim extends App {
  Config.vcssim.compile(TernaryAdderFP8_Soft(withFF=false)).doSim { dut =>
    // simulation process
    dut.clockDomain.forkStimulus(2)
    // simulation code
    for (clk <- 0 until 100) {
      // test case
      if (clk >= 10 && clk < 90) {
        dut.io.X   #= 14
        dut.io.Y   #= 58
        dut.io.Z   #= 196
        dut.io.Cin #= false
      } else {
        dut.io.X   #= 14
        dut.io.Y   #= 58
        dut.io.Z   #= 196
        dut.io.Cin #= false
      }
      dut.clockDomain.waitRisingEdge()    // sample on rising edge
    }
    sleep(50)
  }
}
