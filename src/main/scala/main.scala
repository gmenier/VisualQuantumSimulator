// VQS : Quantum Computing Simulation
// Gildas Ménier
// 2020
// gildas.menier@univ-ubs.fr



import Vqs._
import Vqs.operators._

object main {

      def main(args: Array[String]): Unit = {

        QStudy.caseQFT()

      }

}


/*

object main {

  def main(args : Array[String]): Unit = {

    // QUtils.removeImages

    QReg.setDefaultDrawNOPhaseNormalization()

    // QReg.setDefaultDrawAll()
    // QReg.setDefaultUseASCII()

    val rr: QReg = QReg(8)

    rr.trace()

    rr.init(1)
    rr - H(0) - X(0) - Ry(0,-145) - C(X(1),0) - Rz(3,-195.45) - C(X(1),0) - <() //- C(X(1),0)


    rr.end()



  } // main

} // Main
*/