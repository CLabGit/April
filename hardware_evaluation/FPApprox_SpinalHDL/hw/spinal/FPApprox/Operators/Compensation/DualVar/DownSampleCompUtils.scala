package FPApprox.Operators.Compensation.DualVar

import spinal.core._
import spinal.core.sim._
import FPApprox.Config
import scala.language.postfixOps


class DownSampleComp_LNS_Mult(MantWidth: Int) {

  // MARK: INIT Values here are provided by the April Software Framework

  val INIT_list: List[BigInt] = MantWidth match {
    case 3 => List(
      BigInt("000c3e3e7c783000", 16)      // CompMant[0]
    )

    case 4 => List(
      BigInt("003e706242465c00", 16),     // CompMant[0]
      BigInt("00000e1c3c382000", 16),     // CompMant[1]
    )

    case 5 => List(
      BigInt("0f634b17a894f2f0", 16),     // CompMant[0]
      BigInt("001e3a7262467c00", 16),     // CompMant[1]
      BigInt("0000040c1c380000", 16),     // CompMant[2]
    )

    case 6 => List(
      BigInt("3953b6e483336dce", 16),     // CompMant[0]
      BigInt("06316f5328a6b670", 16),     // CompMant[1]
      BigInt("000e1e3262647800", 16),     // CompMant[2]
      BigInt("0000000c1c180000", 16),     // CompMant[3]
    )

    case 7 => List(
      BigInt("67989966659a95aa", 16),     // CompMant[0]
      BigInt("1f4b2e84e3b3eccc", 16),     // CompMant[1]
      BigInt("0039675348263670", 16),     // CompMant[2]
      BigInt("00061e3222647800", 16),     // CompMant[3]
      BigInt("0000000c1c180000", 16),     // CompMant[4]
    )

    case 10 => List(
      BigInt("0000020004082000", 16),     // CompMant[0]
      BigInt("fefefef8fce8e000", 16),     // CompMant[1]
      BigInt("54aa55af5bb75f3f", 16),     // CompMant[2]
      BigInt("3332cdca3d2ad5aa", 16),     // CompMant[3]
      BigInt("0f696a0cfb93accc", 16),     // CompMant[4]
      BigInt("0019275b50263670", 16),     // CompMant[5]
      BigInt("00061e3a32647800", 16),     // CompMant[6]
      BigInt("000000040c180000", 16),     // CompMant[7]
    )

    case _ => throw new IllegalArgumentException("The given MantWidth is not supported.")
  }

}




class DownSampleCompUtils {}