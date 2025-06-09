package FPApprox.SA.CommonSA

import spinal.core._
import spinal.core.sim._
import FPApprox.Config
import scala.language.postfixOps
import FPApprox.SA.MAC.MAC_FP8Any



case class SystolicArray_FP8Any_Comp(Row: Int, Col: Int, ExpoWidth: Int, MantWidth: Int, UseTerAddOpt: Boolean, MultWithFF: Boolean=true) extends Component {

  val TotalWidth = 1 + ExpoWidth + MantWidth    // 8
  val ExpoWidthPSum = ExpoWidth + 3
  val MantWidthPSum = MantWidth
  val TotalWidthPSum = 1 + ExpoWidthPSum + MantWidthPSum

  val io = new Bundle {
    // data signals
    val DinTop     = in  Vec(Bits(TotalWidth bits), Col)
    val DinLft     = in  Vec(Bits(TotalWidth bits), Row)
    val ResultCout = out Vec(Bits(TotalWidthPSum bits), Row)    // Pass Right

    val PassResult = in  Bool()
  }
  noIoPrefix()

  // generate a table of size Row*Col for DinTop Regs
  val DinTopRegTable = List.tabulate(Row,Col)(_+_).map{ r => r.map{ c => Reg(Bits(TotalWidth bits)).init(B(0)) }}    // for pipelining

  // generate a table of size Row*Col for DinLft Regs
  val DinLftRegTable = List.tabulate(Row,Col)(_+_).map{ r => r.map{ c => Reg(Bits(TotalWidth bits)).init(B(0)) }}    // for pipelining

  // MACs
  val MACs = List.tabulate(Row,Col)(_+_).map{ r => r.map{ c =>
    MAC_FP8Any(ExpoWidth=ExpoWidth, MantWidth=MantWidth, ExpoWidthPSum=ExpoWidthPSum, MantWidthPSum=MantWidthPSum, UseTerAddOpt=UseTerAddOpt, MultWithFF=MultWithFF)
  }}


  // * Connecting DinTopRegTable
  for (c <- 0 until Col) {
    DinTopRegTable(0)(c) := io.DinTop(c)
    for (r <- 1 until Row) {
      DinTopRegTable(r)(c) := DinTopRegTable(r-1)(c)
    }
  }


  // * Connecting DinLftRegTable
  for (r <- 0 until Row) {
    DinLftRegTable(r)(0) := io.DinLft(r)
    for (c <- 1 until Col) {
      DinLftRegTable(r)(c) := DinLftRegTable(r)(c-1)
    }
  }


  // * Multiplying
  for (r <- 0 until Row) {
    for (c <- 0 until Col) {
      MACs(r)(c).io.Iact := DinTopRegTable(r)(c)
      MACs(r)(c).io.Wght := DinLftRegTable(r)(c)
      MACs(r)(c).io.PassResult := io.PassResult
    }
  }


  // * Result CIN & COUT
  for (r <- 0 until Row) {
    for (c <- 0 until Col) {
      if (c == 0) {
        MACs(r)(c).io.ResultCIN := B(0)
      } else {
        MACs(r)(c).io.ResultCIN := Mux(io.PassResult, MACs(r)(c-1).io.Result, B(0))
      }
    }
  }


  // * Output
  for (r <- 0 until Row) {
    io.ResultCout(r) := Mux(io.PassResult, MACs(r)(Col-1).io.Result, B(0))
  }


  // * Signal Renaming
  for (r <- 0 until Row) {
    for (c <- 0 until Col) {
      DinTopRegTable(r)(c).setName(s"DinTopRegTable_r${r}_c${c}")
      DinLftRegTable(r)(c).setName(s"DinLftRegTable_r${r}_c${c}")
      MACs(r)(c).setName(s"MACs_r${r}_c${c}")
    }
  }

}



object SystolicArray_FP8Any_Comp_GenRTL extends App {
  val ExpoWidth = 3
  val MantWidth = 4
  val Size = 8
  Config.setGenSubDir(s"/SA_Size${Size}_Comp_withFF/E${ExpoWidth}M${MantWidth}")    // MARK: With Compensation (For E4M3, E3M4, E2M5)
  Config.spinal.generateVerilog(SystolicArray_FP8Any_Comp(Row=Size, Col=Size, ExpoWidth=ExpoWidth, MantWidth=MantWidth, UseTerAddOpt=true)).mergeRTLSource()    //.printRtl()
}