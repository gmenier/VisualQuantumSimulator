package Vqs

import QComplex._

object QStudy {

  def caseRandom1()  {

//    Qbit
//    #  v
//    0: 0>—H—D ?

    def oneBit(): Int = {
      val rr = QReg(1)
      // Hadamard + read QBit 0
      rr - H(0) - <(0)
      rr.readQbit(0)
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
      rr.drawAll()
      // Hadamard + read QBit 0 1 2
      rr - H(0) - H(1) - H(2) - <(0) - <(1) - <(2)
      // println(rr)
      // println(rr.render)

      QUtils.binaryToInt(s"${rr.readQbit(2)}${rr.readQbit(1)}${rr.readQbit(0)}")

    } // threeBit

    def test(): Unit = {
      println(
        """ Random 3
          |""".stripMargin)
      val v = (for (i <- 0 until 20000) yield threeBit()).toList
      QUtils.drawIntHistogram(v, 3)

      println()
    } // test

    test()
  }

  def caseRandom8() {

    def eightBit(): Int = {
      val rr = QReg(8)

      (0 until 8) foreach ( i => rr - H(i))
      (0 until 8) foreach ( i => rr - <(i))
     // println(rr)
     // println(rr.render)
      rr.readQbit
    } //

    def test(): Unit = {
      println(
        """ Random 8
          |""".stripMargin)
      val v = (for (i <- 0 until 20000) yield eightBit()).toList
      QUtils.drawIntHistogram(v, 8)

      println()
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
    }
    test()
  }

  def caseBell2() {
    def test(): Unit = {
      val rr = QReg(3)
      rr.drawAll()
      rr.trace(2) // dessine la prog et les registres à chaque étape sur 2 lignes
      rr - H(1) - C(X(2), 1) - H(1)
    }
    test()
  }

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
      val alice = rr.readQbit(0)
      val ep = rr.readQbit(1)
      if (ep == 1) rr - X(2)
      if (alice == 1) rr - Z(2)
      rr - H(2) - Rz(2, -45) - H(2)
      rr - <(2)
      rr.end()
    }
    test()
  } // casTeleport

  def caseKickBack() {
    def test(): Unit = {
      val rr: QReg = QReg(3)
      rr.trace(2)
      rr.init(4)
      rr - H(0) - H(1)
      rr - C(Rz(2,45), 0)
      rr - C(Rz(2,90), 1)
    }
    test()
  } // casDraft

  def caseQFT() {

    def qft(myParam: Int)(thisR: QReg): Unit = {
      val s  = thisR.nbQbits -1
      for( i <- 0 until s) {
        thisR - H(s - i)
        var minAngle: Double = (if (thisR.isInRadians) math.Pi/2 else 90)/math.pow(2,i)
        for(j <- 0 to i) {
          thisR - C(Rz(s-j, minAngle), s-i-1)
          minAngle = minAngle + minAngle
        }
      }
      thisR - H(0)
      // swaps
      for(i <- 0 until (s/2+1)) {
        if (i != (s-i)) thisR - Swap(i, s-i)
      }
    } // qft

    def test(): Unit = {
      val rr: QReg = QReg(5)
      rr.pdf("qft.pdf","Quantum Fourier Transform")
      // rr.drawAll()
      rr.trace(8)
      println("start")
      rr.init(2)
      rr - F("QFT",  qft(myParam=25), "qft1", skipTrace = true)
      rr.end()
    }
    test()
  } // casDraft





  def caseDraft() {

    def bell(msg : String)(thisR: QReg): Unit = {
      thisR - H(0) - C(X(1) , 0)
    }

    def test(): Unit = {
      val rr: QReg = QReg(3)
      rr.pdf("teleport.pdf", "Teleportation")
      rr.trace(2)

      //rr.init(0)
      rr - F("Bell",  bell("?"), "0 and 1 intricated", true, false)
      rr.end()
    }
    test()
  } // casDraft

}
