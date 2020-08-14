// VQS : Quantum Computing Simulation
// Gildas Ménier
// 2020
// gildas.menier@univ-ubs.fr



import Vqs._
import Vqs.operators._


object main {

  def main(args : Array[String]): Unit = {
    // QUtils.removeImages
    //QStudy.caseQFT()




    val rr: QReg = QReg(2)
    rr.drawAll()
    rr.trace(2)

    rr.init(1)

    rr - H(0) - C(X(1),0)

    rr.end()








  } // main

} // Main
