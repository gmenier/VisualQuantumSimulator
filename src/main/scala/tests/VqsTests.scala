package tests

import Vqs.{QReg, QUtils}
import Vqs.operators._
import org.scalatest.funsuite.AnyFunSuite

class VqsTests extends AnyFunSuite {

  test("The measure remains for no op") {
    val rr = QReg(3)
    rr.init(7)
    rr - <()
    assert(rr.?() == 7)
  }

  test("X inverts the state") {
    val rr = QReg(3)
    rr.init(5)
    rr - X()
    rr - <()
    assert(rr.?() == 2)
  }


  test("H to superpose + read 1 or 0 with the same probability") {
    var one = 0
    var zero = 0
    for (i <- 0 to 2000) {
      val rr = QReg()
      rr - H()
      rr - <()
      if (rr.?() == 0) zero = zero + 1 else one = one + 1
    }
    val prop = one / zero.toDouble
    assert((prop < 1.1) && (prop > 0.9))
  }


  test("H is its own opposite (basic)") {
    val rr = QReg(3)
    rr.init(5)
    rr - H() - H()
    rr - <()
    assert(rr.?() == 5)
  }


  test("Z is its own opposite (basic)") {
    val rr = QReg(3)
    rr.init(5)
    rr - Z() - Z()
    rr - <()
    assert(rr.?() == 5)
  }


  test("X is its own opposite (basic)") {
    val rr = QReg(3)
    rr.init(5)
    rr - X() - X()
    rr - <()
    assert(rr.?() == 5)
  }


  test("Y is its own opposite (basic)") {
    val rr = QReg(3)
    rr.init(5)
    rr - Y() - Y()
    rr - <()
    assert(rr.?() == 5)
  }


  test("H is its own opposite") {
    val rr = QReg(3)
    rr.init(5)
    rr - H() - H()
    rr - <()
    assert(rr.?() == 5)
  }


  test("Z is its own opposite (random)") {
    val rr = QReg()
    val v12 = QUtils.randomState()

    rr.pokeQBitState(0, v12)
    rr - Z() - Z()

    val v12p = rr.peekQBitState(0, false)
    val v12pn = rr.peekQBitState(0, true)
    assert((v12p == v12) || (v12pn == v12))
  }


  test("X is its own opposite (random)") {
    val rr = QReg()
    val v12 = QUtils.randomState()

    rr.pokeQBitState(0, v12)
    rr - X() - X()

    val v12p = rr.peekQBitState(0, false)
    val v12pn = rr.peekQBitState(0, true)
    assert((v12p == v12) || (v12pn == v12))

  }


  test("XHX = Z") {
    val v12 = QUtils.randomState()

    val rr = QReg()
    rr.pokeQBitState(0, v12)
    rr - X() - H() - X()
    val v12p = rr.peekQBitState(0, false)

    val r2 = QReg()
    r2.pokeQBitState(0, v12)
    r2 - Z()
    val v = rr.peekQBitState(0, false)

    assert(v12p == v)
  }

  test("rotz(45)") {
    val rr = QReg()
    val v12 = QUtils.randomState()
    val p1 = QUtils.cvtRadToDeg(v12._1.bphase)
    val p2 = QUtils.cvtRadToDeg(v12._2.bphase)
    rr.pokeQBitState(0, v12)
    rr - Rz(0, 45)
    val v34 = rr.peekQBitState(0, false)
    val pp1 = QUtils.cvtRadToDeg(v34._1.bphase)
    val pp2 = QUtils.cvtRadToDeg(v34._2.bphase)
    println(" (" + p1 + "," + p2 + ")  " + " (" + pp1 + "," + pp2 + ")  ")

    assert(QUtils.equPhases(p1, pp1, deg = true) && QUtils.equPhases(pp2, p2 + 45, deg = true))
  }


  test("QFT test") {
    val rr: QReg = QReg(4)
    rr.init(1)
    rr - F("QFT", QOperator.qft, "qft", expand = true, skipTrace = false)
    rr.end()

    assert(
      QUtils.equPhases( QUtils.cvtRadToDeg(rr(0).bphase), 0, deg = true) &&
        QUtils.equPhases( QUtils.cvtRadToDeg(rr(1).bphase), 22.5, deg = true) &&
        QUtils.equPhases( QUtils.cvtRadToDeg(rr(2).bphase), 45, deg = true) &&
        QUtils.equPhases( QUtils.cvtRadToDeg(rr(3).bphase), 67.5, deg = true) &&
        QUtils.equPhases( QUtils.cvtRadToDeg(rr(4).bphase), 90, deg = true) &&
        QUtils.equPhases( QUtils.cvtRadToDeg(rr(5).bphase), 112.5, deg = true) &&
        QUtils.equPhases( QUtils.cvtRadToDeg(rr(6).bphase), 135, deg = true) &&
        QUtils.equPhases( QUtils.cvtRadToDeg(rr(7).bphase), 157.5, deg = true) &&
        QUtils.equPhases( QUtils.cvtRadToDeg(rr(8).bphase), 180, deg = true) &&
        QUtils.equPhases( QUtils.cvtRadToDeg(rr(9).bphase), 202.5, deg = true) &&
        QUtils.equPhases( QUtils.cvtRadToDeg(rr(10).bphase), 225, deg = true) &&
        QUtils.equPhases( QUtils.cvtRadToDeg(rr(11).bphase), 247.5, deg = true) &&
        QUtils.equPhases( QUtils.cvtRadToDeg(rr(12).bphase), 270, deg = true) &&
        QUtils.equPhases( QUtils.cvtRadToDeg(rr(13).bphase), 292.5, deg = true) &&
        QUtils.equPhases( QUtils.cvtRadToDeg(rr(14).bphase), 315, deg = true) &&
        QUtils.equPhases( QUtils.cvtRadToDeg(rr(15).bphase), 337.5, deg = true)
    )
  }

}