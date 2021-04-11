package Vqs
// VQS : Quantum Computing Simulation
// Gildas Ménier
// 2020
// gildas.menier@univ-ubs.fr

import Vqs.complex.QComplex
import Vqs.operators._

object QStudy {

  def caseRandom1()  {

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



  def caseSuperDenseCoding() {

    def superDense2(vcode : Int): Unit = {
      val rr: QReg = QReg(2)

      val alice = 1
      val bob = 0

      println("__________________________________\n")

      // Builds a EPR pair
      println("Alice (#1) and Bob (#0) share an EPR pair")
      rr.trace(1)

      rr - $(alice,"Alice") - $(bob,"Bob")

      rr - H(bob) - C(Not(alice), bob)

      // Alice performs a coding
      println("Alice encodes the value "+vcode)

      vcode match {
        case 0 =>
        case 1 => rr - X(alice)
        case 2 => rr - Z(alice)
        case 3 => rr - Y(alice)
      }


      println("  Alice sends its QBit to Bob")

      rr - Label("Alice -> Bob")

      println("  Bob decodes the QBit from Alice")
      rr - C(Not(bob), alice ) - H(alice)

      println("  Bob performs a measure")
      rr - <()

      println("Bob reads "+rr.?())

      println(rr.render)

      rr.end()
    }

    def test() {
      println("Super Dense Coding\n")
      superDense2(0)
      superDense2(1)
      superDense2(2)
      superDense2(3)
    }
    test()
  } // caseSuperDenseCoding

  def caseTeleport() {
    def test(): Unit = {
      val rr: QReg = QReg(3)
      // rr.drawAll()
      rr.pdf("teleport.pdf", "Teleportation")
      rr.trace(2)
      rr - H(1) - C( X(2), 1)
      rr - H(0) - Rz(0, 45) - H(0)
      rr - C(X(1), 0) - H(0)
      rr - <(0) - <(1)
      val alice = rr.readMQBit(0)
      val ep = rr.readMQBit(1)
      if (ep == 1) rr - X(2)
      if (alice == 1) rr - Z(2)
      rr - H(2) - Rz(2, -45) - H(2)
      rr - <(2)
      rr.end()
    }
    test()
  } // casTeleport
  def caseTeleporte() {

    def teleporte(): Unit = {

      // QReg.setDefaultDrawAll()

      val rr: QReg = QReg(3)

      rr.trace(1)


      val bob   = 0
      val alice = 1
      val phi   = 2

      /*
      rr.setQBitLabel(bob, "Bob")
      rr.setQBitLabel(alice, "Alice")
      rr.setQBitLabel(phi, "Phi")
       */
      rr - $(bob,"Bob") - $(alice, "Alice") - $(phi, "Phi")


      println("__________________________________\n")

      // Builds a EPR pair
      println("Alice (#1) and Bob (#0) share an EPR pair\n")
      println("Alice has the QBit phi (#2) with the unknown state of ")

      // rr.init(0)

      // rr.computeStateGivenQbits()

      val phiState : (QComplex, QComplex) = QUtils.randomState()

      println(phiState+"\n\n")

      // rr.pokeQBitState(phi, phiState)
      rr - |>(phi, phiState)


      println(rr)
      rr - H(bob) - C(Not(alice), bob)

      // Alice prepares  the teleportation
      rr - C(Not(alice), phi) - H(phi)

      // Alice measures her QBits
      rr - <(alice) - <(phi)

      val measure  = ""+ rr.?(phi) + rr.?(alice)

      println("Alice reads |"+ measure +">")

      measure match {
        case "00" =>
        case "01" =>  rr - X(bob)
        case "10" =>  rr - Z(bob)
        case "11" =>  rr - Y(bob)
      }

      println(rr.render)

      println(rr)

      println("Bob State is : " + rr.peekQBitState(bob))


      rr.end()
    }

    def test() {
      println("Teleportation\n")
      teleporte()
    }
    test()
  } // caseSuperDenseCoding





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
      rr.trace(1)
      rr.init(3)
      rr - C(Swap(0,2),1)
      rr - <()
      rr.end()
    }
    test()
  } // caseDraft


  def caseAdd() {


    def test(): Unit = {
      val rr: QReg = QReg(4)
      // rr.pdf("test.pdf", "test")
      rr.trace(2)

      rr.init(1)

      rr - H(2) - Rz(2, 45)
 //     rr - H(3) - Rz(3, 90)
      rr - CL( Not(3), List(0,1,2))
      rr - CL( Not(2), List(0,1))
      rr - C( Not(1), 0)
      rr - Not(0)

      rr.end()
    }
    test()

  } // caseAdd


}
