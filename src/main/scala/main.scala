// VQS : Quantum Computing Simulation
// Gildas Ménier
// 2020
// gildas.menier@univ-ubs.fr



import Vqs._


object main {

  def main(args : Array[String]): Unit = {
    //QUtils.removeImages
    //QStudy.caseQFT()

    val rr: QReg = QReg(4)
    rr.trace(2)

    rr.init(1)

    rr - QFT()

    rr.end()




  } // main

} // Main
