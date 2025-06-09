package FPApprox.Operators.Compensation.DualVar

import spinal.core._
import spinal.core.sim._
import FPApprox.Config
import scala.language.postfixOps
import FPApprox.Primitives.{LUT6_Soft, LUT6_Hard}


// MARK: Compensation Logic for FPAny_LNS_Mult utilizing Down-Sample
case class FPAny_LNS_Mult_Comp_DnSp(MantWidth: Int, CompWidth: Int, INIT_list: List[BigInt]) extends Component {
  val io = new Bundle {
    val MantX    = in  UInt(3 bits)
    val MantY    = in  UInt(3 bits)
    val MantComp = out UInt(CompWidth bits)
  }
  noIoPrefix()

  val LUT_input = io.MantY.asBits ## io.MantX.asBits

  // * All LUTs used for Compensation
  val LUTs = (0 until CompWidth).map{ bit =>
    // new LUT6_Soft(INIT=INIT_list(bit))
    new LUT6_Hard(INIT=INIT_list(bit))
  }

  for (bit <- 0 until CompWidth) {
    LUTs(bit).io.I := LUT_input
    io.MantComp(bit) := LUTs(bit).io.O
  }

}



object FPAny_LNS_Mult_Comp_DnSp_RTL extends App {
  Config.setGenSubDir("/Comp")

  // val MantWidth = 3    // MARK: LUT=1
  // val MantWidth = 4    // MARK: LUT=2
  val MantWidth = 5    // MARK: LUT=3
  val DnSpComp = new DownSampleComp_LNS_Mult(MantWidth=MantWidth)

  Config.spinal.generateVerilog(FPAny_LNS_Mult_Comp_DnSp(
    MantWidth=MantWidth, CompWidth=DnSpComp.INIT_list.length, INIT_list=DnSpComp.INIT_list
  )).printRtl()
}



object FPAny_LNS_Mult_Comp_DnSp_Sim extends App {

  val MantWidth = 4
  val DnSpComp = new DownSampleComp_LNS_Mult(MantWidth=MantWidth)

  Config.vcssim.compile(FPAny_LNS_Mult_Comp_DnSp(
      MantWidth=MantWidth, CompWidth=DnSpComp.INIT_list.length, INIT_list=DnSpComp.INIT_list)
  ).doSim { dut =>
    // simulation process
    dut.clockDomain.forkStimulus(2)
    // simulation code
    for (clk <- 0 until 100) {
      // test case
      if (clk >= 10 && clk < 10+64) {
        dut.io.MantX #= (clk-10) % 8
        dut.io.MantY #= (clk-10) / 8
      } else {
        dut.io.MantX #= 0
        dut.io.MantY #= 0
      }
      dut.clockDomain.waitRisingEdge()    // sample on rising edge
    }
    sleep(50)
  }
}

