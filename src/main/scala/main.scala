// VQS : Quantum Computing Simulation
// Gildas Ménier
// 2020
// gildas.menier@univ-ubs.fr

import Vqs._
import Vqs.operators.{$, <, C, H, Rz, X, Y, Z}




object main {

     def main(args: Array[String]): Unit = {
         QReg.setDefaultUseColor()
         QReg.setDefaultUseASCII()

        QReg.setDefaultDrawAll()
        QReg.setDefaultDrawBitCircles()

        // QReg.setDefaultDrawNOPhaseNormalization()

         QStudy.caseQFT()

        // simple example
 /*       QReg.setDefaultDrawAll()
        QReg.setDefaultDrawBitCircles()

        val rr: QReg = QReg(3)
        rr.trace(2)
        rr.pdf("test.pdf", "Test")

        rr.init(6)
        rr - H(0) - X(1) - Z(2)
        rr.end()
*/

      }
}

