package FPApprox.SA.MAC

import spinal.core._
import spinal.core.sim._
import FPApprox.Config
import scala.language.postfixOps
import scala.math.pow
import FPApprox.Operators.Arithmetic.{TernaryAdderFP8_Hard, TernaryAdderFP8_Soft, fpany_adder_widen}
import FPApprox.Operators.Compensation.DualVar.{DownSampleComp_LNS_Mult, FPAny_LNS_Mult_Comp_DnSp}


case class MAC_FP8Any(ExpoWidth: Int, MantWidth: Int, ExpoWidthPSum: Int, MantWidthPSum: Int, UseTerAddOpt: Boolean, MultWithFF: Boolean=true) extends Component {

  val NegB_for_Expo = -(pow(2, ExpoWidth-1) - 1).toInt

  val TotalWidth = 1 + ExpoWidth + MantWidth    // 8
  val TotalWidthPSum = 1 + ExpoWidthPSum + MantWidthPSum

  val io = new Bundle {
    val Iact       = in  Bits(TotalWidth bits)
    val Wght       = in  Bits(TotalWidth bits)
    val Result     = out Bits(TotalWidthPSum bits)

    val PassResult = in  Bool()
    val ResultCIN  = in  Bits(TotalWidthPSum bits)
  }
  noIoPrefix()

  // * Ternary Adder
  val Mult = UseTerAddOpt match {
    case true  => new TernaryAdderFP8_Hard(withFF=MultWithFF)
    case false => new TernaryAdderFP8_Soft(withFF=MultWithFF)
  }

  Mult.addAttribute("DONT_TOUCH=\"yes\"")


  // * Compensation
  if (MantWidth > 2) {
    // ** For E4M3, E3M4, E2M5
    // Current Down-Sample factor is set as 3
    val DnSpCompList = new DownSampleComp_LNS_Mult(MantWidth=MantWidth)
    val DnSpComp = FPAny_LNS_Mult_Comp_DnSp(
      MantWidth=MantWidth, CompWidth=DnSpCompList.INIT_list.length, INIT_list=DnSpCompList.INIT_list
    )
    DnSpComp.io.MantX := io.Iact(MantWidth-1 downto MantWidth-3).asUInt
    DnSpComp.io.MantY := io.Wght(MantWidth-1 downto MantWidth-3).asUInt
    val DnSpCompResized = DnSpComp.io.MantComp.asBits.resize(MantWidth bits)
    Mult.io.Z := S(NegB_for_Expo, (1 + ExpoWidth) bits).asBits ## DnSpCompResized
  } else {
    // ** For E5M2
    Mult.io.Z := S(NegB_for_Expo, (1 + ExpoWidth) bits).asBits ## B(0, MantWidth bits)
  }


  Mult.io.X   := io.Iact
  Mult.io.Y   := io.Wght
  Mult.io.Cin := False


  val AccuReg = Reg(Bits(TotalWidthPSum bits)).init(B(0))

  // * Adder
  val FPAnyAdd = new fpany_adder_widen(ExpoWidthMult=ExpoWidth, MantWidthMult=MantWidth, ExpoWidthPSum=ExpoWidthPSum, MantWidthPSum=MantWidthPSum)
  FPAnyAdd.io.a := Mult.io.Sum
  FPAnyAdd.io.b := AccuReg

  // * Simple Control
  when(io.PassResult) {
    AccuReg := io.ResultCIN
  } otherwise {
    AccuReg := FPAnyAdd.io.r
  }

  io.Result := AccuReg

}


object MAC_FP8Any_Gen extends App {
  // MARK: Vivado Synthesis with Flow_AreaOptimized_high; Freq=250MHz
  Config.setGenSubDir("/MAC")
  Config.spinal.generateVerilog(MAC_FP8Any(ExpoWidth=4, MantWidth=3, ExpoWidthPSum=4+3, MantWidthPSum=3, UseTerAddOpt=true, MultWithFF=true)).printRtl().mergeRTLSource()    // for Vivado impl test
}


object MAC_FP8Any_GenAll extends App {
  for (e <- 2 to 5) {
    val ExpoWidth = e
    val MantWidth = 8 - 1 - e
    println(MantWidth)
    Config.setGenSubDir(s"/MAC_test/E${ExpoWidth}_M${MantWidth}")
    Config.spinal.generateVerilog(
      MAC_FP8Any(ExpoWidth=ExpoWidth, MantWidth=MantWidth, ExpoWidthPSum=ExpoWidth+3, MantWidthPSum=MantWidth, UseTerAddOpt=true, MultWithFF=true)
    ).printRtl().mergeRTLSource()
  }
}