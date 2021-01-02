// VQS : Quantum Computing Simulation
// Gildas Ménier
// 2020
// gildas.menier@univ-ubs.fr



import Vqs._
import Vqs.operators._

/*
import Vqs.complex.QComplex.i
import Vqs.complex.{QM, QV}
import Vqs.operators._

import scala.math.{cos, sin}
*/



object main {

  def main(args : Array[String]): Unit = {
    // QUtils.removeImages
    // QStudy.caseQFT()



    val rr: QReg = QReg(1)
    // rr.drawAll()


    rr.trace(useASCII = true)


    // rr.init()

    rr - H(0) - X() - <() //- C(X(1),0)



    rr.end()








  } // main

} // Main
