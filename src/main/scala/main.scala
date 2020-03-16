// VQS : Quantum Computing Simulation
// Gildas Ménier
// 2020
// gildas.menier@univ-ubs.fr






object main {

  def main(args : Array[String]): Unit = {
    // QUtils.removeImages
    // QStudy.caseQFT()

    import Vqs._
    val rr = QReg(4)
    rr - F("QFT",  QOperator.qft, expand = true)
    println(rr.render)

  } // main

} // Main
