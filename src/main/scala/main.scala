// VQS : Quantum Computing Simulation
// Gildas Ménier
// 2020
// gildas.menier@univ-ubs.fr

import Vqs._
import Vqs.operators.{C, H, Rz}




object main {

      def main(args: Array[String]): Unit = {
         QReg.setDefaultUseColor()
         //QReg.setDefaultUseASCII()

        // QReg.setDefaultDrawAll()
        // QReg.setDefaultDrawBitCircles()

        // QReg.setDefaultDrawNOPhaseNormalization()



         QStudy.caseQFT()

        // simple example

        /*
        val rr: QReg = QReg(3)
        rr.trace(2)
        rr.init(2)
        rr - H(0) - H(1) - H(2)
        rr.end()
         */

      }
}

