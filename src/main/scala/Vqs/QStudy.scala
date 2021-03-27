package Vqs
// VQS : Quantum Computing Simulation
// Gildas Ménier
// 2020
// gildas.menier@univ-ubs.fr

import Vqs.complex.QComplex
import Vqs.operators._

object QStudy {

  def caseRandom1()  {

//    Qbit
//    #  v
//    0: 0>—H—D ?

    def oneBit(): Int = {
      val rr = QReg(1)
      // Hadamard + read QBit 0
      rr - H(0) - <(0)
      rr.readMQBit(0)
    }

    def test(): Unit = {
      println(
        """ Random 1 QBit
          |    Qbit
          |    #  v
          |    0: 0>—H—D ?
          |""".stripMargin)
      for( i <- 0 until 20)  {
        print(oneBit()+" ")
      }
      println()
    } // test

    test()
  }

  def caseRandom3() {

    def threeBit(): Int = {
      val rr = QReg(3)
      rr - H() - <()
      return rr.?()
    } // threeBit

    def test(): Unit = {
      println("Random 3")
      val v = (for (i <- 0 until 20000) yield threeBit()).toList
      QUtils.drawIntHistogram(v, 3)
    } // test

    test()
  }

  def caseRandom8() {

    def eightBit(): Int = {
      val rr = QReg(8)
      rr - H() - <()
      return rr.?()
    } //

    def test(): Unit = {
      println("Random 8")
      val v = (for (i <- 0 until 102400) yield eightBit()).toList
      QUtils.drawIntHistogram(v, 8)
    } // test

    test()
  }

  def caseBell1() {
    def test(): Unit = {
      val rr = QReg(2)
      rr.drawAll()

      rr.write(2)

      println(rr.render); println(rr)

      rr - H(0)
      println(rr.render); println(rr)
      rr - C(X(1),0)

      println(rr.render); println(rr)
      rr.end()
    }
    test()
  }

  def caseBell2() {
    def test(): Unit = {
      val rr = QReg(3)
      rr.drawAll()
      rr.trace(2) // dessine la prog et les registres à chaque étape sur 2 lignes
      rr - H(1) - C(X(2), 1) - H(1)
      rr.end()
    }
    test()
  }


  def caseKickBack() {
    def test(): Unit = { //
      val rr: QReg = QReg(3)
      rr.trace(2)
      rr.init(4)
      rr - H(0) - H(1)
      rr - C(Rz(2,45), 0)
      rr - C(Rz(2,90), 1)
      rr.end()
    }
    test()
  } // caseKickBack



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






  def caseSwap(): Unit = {

    def test(): Unit = {
      val rr = new QReg(9)
      rr.trace()

      for( i <- 0 until 9)  rr - C(Not(2),i)

      rr.end()
    }

    test()

  }

  def caseDraft() {


    def test(): Unit = {
      val rr: QReg = QReg(3)
      // rr.pdf("test.pdf", "test")
      rr.trace(2)
      rr.pokeQBitState(1, (0,1))

      rr - CL(Not(0),List(1,2))
      println(rr.render)
      rr.end()
    }
    test()
  } // caseDraft


  def caseAdd() {

    def test(): Unit = {
      val rr: QReg = QReg(3)
      // rr.pdf("test.pdf", "test")
      rr.trace(2)
      rr.pokeQBitState(1, (0,1))

      rr - CL(Not(0),List(1,2))
      println(rr.render)
      rr.end()
    }
    test()

  } // caseAdd


}
