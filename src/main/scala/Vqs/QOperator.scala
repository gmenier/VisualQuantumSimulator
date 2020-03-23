package Vqs
// VQS : Quantum Computing Simulation
// Gildas Ménier
// 2020
// gildas.menier@univ-ubs.fr


import QComplex._
import QV._
import QM._
import scala.math._
import io.AnsiColor._



trait QOperator {

  var thisR : QReg = _

  def render(pad: QPad);
  def idxBit : Int;
  def op(qbit : QV):QV ;

  var leaveATrace : Boolean = true

  def init() {}

  def setRegister(thisR_ : QReg): Unit = {
    thisR = thisR_
  }

  val Id = QM(
    QV(1,0),
    QV(0,1)
  )
}


case class Label(label:String) extends QOperator {

  leaveATrace = false

  def render(pad: QPad): Unit = {
    pad.label(label)
    (label+"  ").indices.foreach( _ => pad.nextCol())
  }

  def idxBit = -1 // label

  def op(qbit: QV) : QV = Id.mult(qbit)
} // Label

case class VLabel(label:String) extends QOperator {

  leaveATrace = false

  def render(pad: QPad): Unit = {
    pad.vlabel(label)
    pad.nextCol2();
  }

  def idxBit = -1 // label

  def op(qbit: QV) : QV = Id.mult(qbit)
} // Label


case class |(nb : Int = 4) extends QOperator {

  leaveATrace = false

  def render(pad: QPad): Unit = {
    for (i <- (0 until nb)) pad.nextCol();
  }

  def idxBit = -1 // label

  def op(qbit: QV) : QV = Id.mult(qbit)
} // Label




// Hadamard
// p = All for all QBits
case class H(idx:Int = QReg.All) extends QOperator {

  override def init(): Unit = {
    if ( ((idx < 0) && (idx != QReg.All)) ||
      (idx >= thisR.nbQbits))  QReg.notifyError("Incorrect #QBit for H")
  }

  val sq2 = sqrt(2)

  def render(pad: QPad): Unit = {
    pad.at(idx, "H")
    pad.nextCol2()
  }

  def idxBit = idx;

  def op(qbit: QV) : QV = QM(
    QV(1/sq2,1/sq2),
    QV(1/sq2,-1/sq2)
  ).mult(qbit)
} // H


case class X(idx:Int = QReg.All) extends QOperator {

  override def init(): Unit = {
    if ( ((idx < 0) && (idx != QReg.All)) ||
      (idx >= thisR.nbQbits))  QReg.notifyError("Incorrect #QBit for X")
  }

  def render(pad: QPad): Unit = {
    pad.at(idx, "X")
    pad.nextCol2()
  }

  def idxBit = idx;

  def op(qbit: QV) : QV = QM(
    QV(0,1),
    QV(1,0)
  ).mult(qbit)
} // X

case class Not(idx:Int = QReg.All) extends QOperator {

  override def init(): Unit = {
    if ( ((idx < 0) && (idx != QReg.All)) ||
      (idx >= thisR.nbQbits))  QReg.notifyError("Incorrect #QBit for Not")
  }

  def render(pad: QPad): Unit = {
    pad.at(idx, "X")
    pad.nextCol2()
  }

  def idxBit = idx;

  def op(qbit: QV) : QV = QM(
    QV(0,1),
    QV(1,0)
  ).mult(qbit)
} // X

case class Y(idx:Int = QReg.All) extends QOperator {

  override def init(): Unit = {
    if ( ((idx < 0) && (idx != QReg.All)) ||
      (idx >= thisR.nbQbits))  QReg.notifyError("Incorrect #QBit for Y")
  }

  def render(pad: QPad): Unit = {
    pad.at(idx, "Y")
    pad.nextCol2()
  }

  def idxBit = idx;

  def op(qbit: QV) : QV = QM(
    QV(0, -i),
    QV(i, 0)
  ).mult(qbit)
} // Y = Y180

case class Z(idx:Int = QReg.All) extends QOperator {

  override def init(): Unit = {
    if ( ((idx < 0) && (idx != QReg.All)) ||
      (idx >= thisR.nbQbits))  QReg.notifyError("Incorrect #QBit for Z")
  }

  def render(pad: QPad): Unit = {
    pad.at(idx, "Z")
    pad.nextCol2()
  }

  def idxBit = idx;

  def op(qbit: QV) : QV = QM(
    QV(1, 0),
    QV(0, -1)
  ).mult(qbit)
} // Z = Z180


case class S(idx:Int = QReg.All) extends QOperator { // Z90 , S = T2

  override def init(): Unit = {
    if ( ((idx < 0) && (idx != QReg.All)) ||
      (idx >= thisR.nbQbits))  QReg.notifyError("Incorrect #QBit for S")
  }

  def render(pad: QPad): Unit = {
    pad.at(idx, "S")
    pad.nextCol2()
  }

  def idxBit = idx;

  def op(qbit: QV) : QV = QM(
    QV(1, 0),
    QV(0, i)
  ).mult(qbit)
} // S = Z90

case class mS(idx:Int = QReg.All) extends QOperator { // Z-90

  override def init(): Unit = {
    if ( ((idx < 0) && (idx != QReg.All)) ||
      (idx >= thisR.nbQbits))  QReg.notifyError("Incorrect #QBit for mS")
  }

  def render(pad: QPad): Unit = {
    pad.at(idx, "mS")
    pad.nextCol2(); pad.nextCol()
  }

  def idxBit = idx;

  def op(qbit: QV) : QV = QM(
    QV(1, 0),
    QV(0, -i)
  ).mult(qbit)
} // S = Z-90


case class T(idx:Int = QReg.All) extends QOperator { // Z45

  override def init(): Unit = {
    if ( ((idx < 0) && (idx != QReg.All)) ||
      (idx >= thisR.nbQbits))  QReg.notifyError("Incorrect #QBit for T")
  }

  def render(pad: QPad): Unit = {
    pad.at(idx, "T")
    pad.nextCol2()
  }

  def idxBit = idx;

  def op(qbit: QV) : QV = QM(
    QV(1, 0),
    QV(0, cos(Pi/4)+i*sin(Pi/4))
  ).mult(qbit)
} // T = Z45

case class mT(idx:Int = QReg.All) extends QOperator { // Z-45

  override def init(): Unit = {
    if ( ((idx < 0) && (idx != QReg.All)) ||
      (idx >= thisR.nbQbits))  QReg.notifyError("Incorrect #QBit for mT")
  }

  def render(pad: QPad): Unit = {
    pad.at(idx, "mT")
    pad.nextCol2(); pad.nextCol()
  }

  def idxBit = idx;

  def op(qbit: QV) : QV = QM(
    QV(1, 0),
    QV(0, cos(-Pi/4)+i*sin(-Pi/4))
  ).mult(qbit)
} // T = Z-45



case class Rx(p:Int, angle :Double) extends QOperator {

  var o = angle

  override def init(): Unit = {
    if ( ((p < 0) && (p != QReg.All)) ||
      (p >= thisR.nbQbits))  QReg.notifyError("Incorrect #QBit for Rx")
    o = thisR.angle(angle)
  }


  def render(pad: QPad): Unit = {
    pad.at(p, "Rx");
    pad.infol(angle.toString)
    pad.nextCol2();     pad.nextCol2()
    pad.nextCol2()
  }

  def idxBit: Int = p;

  def op(qbit : QV):QV = { // Rx
    QM(
      QV(cos(o/2), -i*sin(o/2)),
      QV(-i*sin(o/2), cos(o/2))
    ).mult(qbit)
  } // Rx
} // Rx

case class Ry(p:Int, angle :Double) extends QOperator {

  var o = angle

  override def init(): Unit = {
    if ( ((p < 0) && (p != QReg.All)) ||
      (p >= thisR.nbQbits))  QReg.notifyError("Incorrect #QBit for Ry")
    o = thisR.angle(angle)
  }


  def render(pad: QPad): Unit = {
    pad.at(p, "Ry");
    pad.infol(angle.toString)
    pad.nextCol2();     pad.nextCol2()
    pad.nextCol2()
  }

  def idxBit: Int = p;

  def op(qbit : QV):QV = { // Ry
    QM(
      QV(cos(o/2), -sin(o/2)),
      QV(sin(o/2), cos(o/2))
    ).mult(qbit)
  } // Ry
} // Ry

case class Rz(p:Int, angle : Double) extends QOperator {

  var o = angle

  override def init(): Unit = {
    if ( ((p < 0) && (p != QReg.All)) ||
      (p >= thisR.nbQbits))  QReg.notifyError("Incorrect #QBit for Rz")
    o = thisR.angle(angle)
  }



  def render(pad: QPad): Unit = {
    pad.at(p, "Rz");
    pad.infol(angle.toString)
    pad.nextCol2();     pad.nextCol2()
    pad.nextCol2()
  }

  def idxBit: Int = p;

  def op(qbit : QV):QV = { // Rz
    /*
    QM(
      QV(cos(-o/2)-i*sin(o/2) , 0                   ),
      QV(0                    , cos(o/2)+i*sin(o/2) )
    ).mult(qbit)
     */
    QM(
      QV(1 , 0               ),
      QV(0 , cos(o)+i*sin(o) )
    ).mult(qbit)

  } // Rz
} // Rz


case class CL(Qop: QOperator, condList_ : List[Int]) extends QOperator {

  val condList: List[Int] = condList_.distinct

  override def init(): Unit = {
    condList.foreach ( p => if ( (p < 0) || (p >= thisR.nbQbits))  QReg.notifyError("Incorrect #QBit for CL( op, list(#))") )
  }

  def render(pad: QPad): Unit = {
    val ll = idxBit :: condList
    val mn = ll.min
    val mx = ll.max
    for(i <- mn*2 to mx*2) pad.atAbs(i,"│")
    for(i <- condList) pad.at(i,"•")
    Qop.render(pad)
  }
  def idxBit: Int = Qop.idxBit;
  def op( qbit : QV):QV = Qop.op(qbit)
} // C


case class C(Qop: QOperator, cond: Int) extends QOperator { // C as in CNOT -> C(Not(Idx), idxp) -> C (X(idx), idxp)

  override def init(): Unit = {
    if ( (cond < 0) || (cond >= thisR.nbQbits))  QReg.notifyError("Incorrect #QBit for C( op, #)")
  }

  def render(pad: QPad): Unit = {
    val condList = List(cond)
    val ll = idxBit :: condList
    val mn = ll.min
    val mx = ll.max
    for(i <- mn*2 to mx*2) pad.atAbs(i,"│")
    for(i <- condList) pad.at(i,"•")
    Qop.render(pad)
  }
  def idxBit: Int = Qop.idxBit;
  def op( qbit : QV):QV = Qop.op(qbit)
} // C



// Swaps the qbits e1 et e2
case class Swap(e1: Int, e2: Int) extends QOperator {

  override def init(): Unit = {
    if ((e1 == e2) || (e1 < 0) || (e2 < 0) ||
      (e1 >= thisR.nbQbits) || (e2 >= thisR.nbQbits)) QReg.notifyError("Incorrect #QBit for Swap Gate")
  }

  def render(pad: QPad): Unit = {
    if (e1 < e2) for(i <- e1*2 to e2*2) pad.atAbs(i,"│")
    else for(i <- e2*2 to e1*2) pad.atAbs(i,"│")
    pad.at(e1,"x")
    pad.at(e2,"x")
    pad.nextCol2()
  }
  def idxBit: Int = -1;
  def op( qbit : QV):QV = Id.mult(qbit)
} // Swap


// lecture d'un QBit (Détermination)
// Z Measure
// idx = ALL pour tous les Qbits
case class <(idx: Int = QReg.All) extends QOperator {

  override def init(): Unit = {
    if (((idx < 0) && (idx != QReg.All)) ||
      (idx >= thisR.nbQbits)) QReg.notifyError("Incorrect #QBit for Measurement <()")
  }

  def render(pad: QPad): Unit = {
    pad.DRenderAt(idx)
    pad.nextCol2(); pad.nextCol2();
    pad.toEnd(idx, ' '/* '═' */)
  }

  def idxBit = idx;

  def op(qbit: QV) : QV = Id.mult(qbit)
} // Label

// function
case class F(name: String, fun: QReg => Unit, msg : String ="", expand : Boolean = false, skipTrace: Boolean = true ) extends QOperator {
  def render(pad: QPad): Unit = {
    pad.atAbs(0,"╓")
    for(i <- 1 to thisR.nbQbits*2) pad.atAbs(i,"║")
    thisR.infoline(name + (if (msg.length > 0) " : " else "" ) + msg)

    pad.atAbs((thisR.nbQbits-1)*2+2, "╙")
    pad.nextCol(); pad.nextCol2();
  }

  def idxBit = 0

  def op(qbit: QV) : QV = Id.mult(qbit)
} // Label

object QOperator {

  // QFT on all QBits
  def qft(thisR: QReg): Unit = {
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
  }

}



