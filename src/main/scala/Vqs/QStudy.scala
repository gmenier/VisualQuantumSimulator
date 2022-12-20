package Vqs
// VQS : Quantum Computing Simulation
// Gildas Ménier
// 2020
// gildas.menier@univ-ubs.fr

import Vqs.complex.QComplex
import Vqs.operators._

object QStudy {


  def caseQFT() {

    def test(): Unit = {
      // QReg.setDefaultRadians()

      QReg.setDefaultDontDrawBitCircles()

      val rr: QReg = QReg(4)

      rr.pdf("qft.pdf","Quantum Fourier Transform")
      // rr.drawAll()
      rr.trace(4)

      rr.init(1)
      rr - F("QFT",  QOperator.qft, "qft", expand = true, skipTrace = false)

      println(rr.render)
      rr.drawCircleImage("qft", zoomparam = 6, text = "QFT")
      rr.end()
    }
    test()
  } // caseQFT


}
