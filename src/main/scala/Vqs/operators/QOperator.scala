package Vqs.operators
// VQS : Quantum Computing Simulation
// Gildas Ménier
// 2020
// gildas.menier@univ-ubs.fr


import Vqs.complex._
import Vqs.complex.QComplex._
import Vqs._
import Vqs.output._

import scala.math._



trait QOperator {

  var thisR : QReg = _ // pointer to the current QRegister

  def render(pad: QPad); // dedicated renderer
  def idxBit : List[Int];

  def op(qbit: QV) : QV = Id.mult(qbit) // identity

  def opLabel : String = "?" ; // label

  var leaveATrace : Boolean = true

  val NoIdx = -1

  def init() {} // called to set thisR

  def alias = this // so you can rename the operator

  def setRegister(thisR_ : QReg): Unit = {
    thisR = thisR_
  }

  val Id = QM(
    QV(1,0),
    QV(0,1)
  ) // by default, Identity
}


// screen composition

/** not an operator : inserts a tab and a string in the rendering */
case class Label(label:String) extends QOperator {

  leaveATrace = false

  def render(pad: QPad): Unit = {
    pad.label(label)
    (label+" ").indices.foreach( _ => pad.nextCol())
  }

  def idxBit = List() // label

} // Label


case class $(nm : Int, label:String) extends QOperator {

  leaveATrace = false

  def render(pad: QPad): Unit = {}

  def idxBit = List() // label

} // Label


case class |>(numQbit : Int, state: (QComplex, QComplex)) extends QOperator {

  leaveATrace = false

  def render(pad: QPad): Unit = {}

  def idxBit =  List() // label

} // Label




/** not an operator : inserts a vertical string in the rendering */
case class VLabel(label:String) extends QOperator {

  leaveATrace = false

  def render(pad: QPad): Unit = {
    pad.vlabel(label)
    pad.nextCol2();
  }

  def idxBit =  List() // label

} // Label

/** not an operator : inserts a tab in the rendering */
case class |(nb : Int = 4) extends QOperator {

  leaveATrace = false

  def render(pad: QPad): Unit = {
    for (i <- (0 until nb)) pad.nextCol();
  }

  def idxBit =  List() // label

} // Label




/**  Hadamard operator. By defaut, applies on Qreg.all (on all QBits) */
case class H(idx:Int = QReg.All) extends QOperator {

  override def init(): Unit = {
    if ( ((idx < 0) && (idx != QReg.All)) ||
      (idx >= thisR.nbQbits))  QReg.notifyError("in H("+idx+") Incorrect #QBit")
  }

  val sq2 = sqrt(2)

  def render(pad: QPad): Unit = {
    pad.at(idx, "H")
    pad.nextCol2()
  }

  override def opLabel = "H"+ (if (idx == QReg.All) "" else "("+thisR.LabelOf(idx) +")")

  def idxBit = List(idx);

  override def op(qbit: QV) : QV = QM(
    QV(1/sq2,1/sq2),
    QV(1/sq2,-1/sq2)
  ).mult(qbit)
} // H

/**  Inversion operator (X). By defaut, applies on Qreg.all (on all QBits) */
case class X(idx:Int = QReg.All) extends QOperator {

  override def init(): Unit = {
    if ( ((idx < 0) && (idx != QReg.All)) ||
      (idx >= thisR.nbQbits))  QReg.notifyError("in X("+idx+") Incorrect #QBit")
  }

  override def opLabel = "X"+ (if (idx == QReg.All) "" else "("+thisR.LabelOf(idx) +")")


  def render(pad: QPad): Unit = {
    pad.at(idx, "X")
    pad.nextCol2()
  }

  def idxBit = List(idx);

  override   def op(qbit: QV) : QV = QM(
    QV(0,1),
    QV(1,0)
  ).mult(qbit)
} // X

/**  Inversion operator (X). By defaut, applies on Qreg.all (on all QBits) */
case class Not(idx:Int = QReg.All) extends QOperator {

  override def init(): Unit = {
    if ( ((idx < 0) && (idx != QReg.All)) ||
      (idx >= thisR.nbQbits))  QReg.notifyError("in Not("+idx+") Incorrect #QBit")
  }

  override def opLabel = "Not"+ (if (idx == QReg.All) "" else "("+thisR.LabelOf(idx) +")")


  def render(pad: QPad): Unit = {
    pad.at(idx, "X")
    pad.nextCol2()
  }

  def idxBit = List(idx);

  override   def op(qbit: QV) : QV = QM(
    QV(0,1),
    QV(1,0)
  ).mult(qbit)
} // X

/**  Pauli Y. By defaut, applies on Qreg.all (on all QBits) */
case class Y(idx:Int = QReg.All) extends QOperator {

  override def init(): Unit = {
    if ( ((idx < 0) && (idx != QReg.All)) ||
      (idx >= thisR.nbQbits))  QReg.notifyError("in Y("+idx+") Incorrect #QBit")
  }

  override def opLabel = "Y"+ (if (idx == QReg.All) "" else "("+thisR.LabelOf(idx) +")")

  def render(pad: QPad): Unit = {
    pad.at(idx, "Y")
    pad.nextCol2()
  }

  def idxBit = List(idx);

  override  def op(qbit: QV) : QV = QM(
    QV(0, -i),
    QV(i, 0)
  ).mult(qbit)
} // Y = Y180

/**  Pauli Z - Phase. By defaut, applies on Qreg.all (on all QBits) */
case class Z(idx:Int = QReg.All) extends QOperator {

  override def init(): Unit = {
    if ( ((idx < 0) && (idx != QReg.All)) ||
      (idx >= thisR.nbQbits))  QReg.notifyError("in Z("+idx+") Incorrect #QBit")
  }

  override def opLabel = "Z"+ (if (idx == QReg.All) "" else "("+thisR.LabelOf(idx) +")")

  def render(pad: QPad): Unit = {
    pad.at(idx, "Z")
    pad.nextCol2()
  }

  def idxBit = List(idx);

  override   def op(qbit: QV) : QV = QM(
    QV(1, 0),
    QV(0, -1)
  ).mult(qbit)
} // Z = Z180

/**  Half Z. By defaut, applies on Qreg.all (on all QBits) */
case class S(idx:Int = QReg.All) extends QOperator { // Z90 , S = T2

  override def init(): Unit = {
    if ( ((idx < 0) && (idx != QReg.All)) ||
      (idx >= thisR.nbQbits))  QReg.notifyError("in S("+idx+") Incorrect #QBit")
  }

  override def opLabel = "S"+ (if (idx == QReg.All) "" else "("+thisR.LabelOf(idx) +")")

  def render(pad: QPad): Unit = {
    pad.at(idx, "S")
    pad.nextCol2()
  }

  def idxBit = List(idx);

  override  def op(qbit: QV) : QV = QM(
    QV(1, 0),
    QV(0, i)
  ).mult(qbit)
} // S = Z90

/**  -Half Z. By defaut, applies on Qreg.all (on all QBits) */
case class mS(idx:Int = QReg.All) extends QOperator { // Z-90

  override def init(): Unit = {
    if ( ((idx < 0) && (idx != QReg.All)) ||
      (idx >= thisR.nbQbits))  QReg.notifyError("in mS("+idx+") Incorrect #QBit")
  }

  override def opLabel = "mS"+ (if (idx == QReg.All) "" else "("+thisR.LabelOf(idx) +")")


  def render(pad: QPad): Unit = {
    pad.at(idx, "mS")
    pad.nextCol2(); pad.nextCol()
  }

  def idxBit = List(idx);

  override  def op(qbit: QV) : QV = QM(
    QV(1, 0),
    QV(0, -i)
  ).mult(qbit)
} // S = Z-90

/**  1/4 Z. By defaut, applies on Qreg.all (on all QBits) */
case class T(idx:Int = QReg.All) extends QOperator { // Z45

  override def init(): Unit = {
    if ( ((idx < 0) && (idx != QReg.All)) ||
      (idx >= thisR.nbQbits))  QReg.notifyError("in T("+idx+") Incorrect #QBit")
  }

  override def opLabel = "T"+ (if (idx == QReg.All) "" else "("+thisR.LabelOf(idx) +")")

  def render(pad: QPad): Unit = {
    pad.at(idx, "T")
    pad.nextCol2()
  }

  def idxBit = List(idx);

  override   def op(qbit: QV) : QV = QM(
    QV(1, 0),
    QV(0, cos(Pi/4)+i*sin(Pi/4))
  ).mult(qbit)
} // T = Z45

/**  -1/4 Z. By defaut, applies on Qreg.all (on all QBits) */
case class mT(idx:Int = QReg.All) extends QOperator { // Z-45

  override def init(): Unit = {
    if ( ((idx < 0) && (idx != QReg.All)) ||
      (idx >= thisR.nbQbits))  QReg.notifyError("in mT("+idx+") Incorrect #QBit")
  }

  override def opLabel = "mT"+ (if (idx == QReg.All) "" else "("+thisR.LabelOf(idx) +")")

  def render(pad: QPad): Unit = {
    pad.at(idx, "mT")
    pad.nextCol2(); pad.nextCol()
  }

  def idxBit = List(idx);

  override  def op(qbit: QV) : QV = QM(
    QV(1, 0),
    QV(0, cos(-Pi/4)+i*sin(-Pi/4))
  ).mult(qbit)
} // T = Z-45


/**  Rotate X */
case class Rx(p:Int = QReg.All, angle :Double) extends QOperator {

  var o = angle

  override def init(): Unit = {
    if ( ((p < 0) && (p != QReg.All)) ||
      (p >= thisR.nbQbits))  QReg.notifyError("in Rx("+p+",...) Incorrect #QBit")
    o = thisR.angle(angle)
  }

  override def opLabel = "Rx"+ (if (p == QReg.All) "" else "("+thisR.LabelOf(p) +","+angle +")")

  def render(pad: QPad): Unit = {
    pad.at(p, "Rx");
    val angleStr = ((angle.toString+" ").replaceAll(".0 ","")+" "*10).substring(0,5)
    pad.infol(angleStr);
    if ( (p< thisR.nbQbits) && (p != QReg.All)) { pad.nextCol(); pad.atAbs(p*2+1, angleStr); pad.previousCol() }
    pad.nextCol2();     pad.nextCol2()
    pad.nextCol2()
  }

  def idxBit: List[Int] = List(p);

  override  def op(qbit : QV):QV = { // Rx
    QM(
      QV(cos(o/2), -i*sin(o/2)),
      QV(-i*sin(o/2), cos(o/2))
    ).mult(qbit)
  } // Rx
} // Rx

/**  Rotate Y */
case class Ry(p:Int = QReg.All, angle :Double) extends QOperator {

  var o = angle

  override def init(): Unit = {
    if ( ((p < 0) && (p != QReg.All)) ||
      (p >= thisR.nbQbits))  QReg.notifyError("in Ry("+p+",...) Incorrect #QBit")
    o = thisR.angle(angle)
  }

  override def opLabel = "Ry"+ (if (p == QReg.All) "" else "("+thisR.LabelOf(p) +","+angle +")")

  def render(pad: QPad): Unit = {
    pad.at(p, "Ry");
    val angleStr = ((angle.toString+" ").replaceAll(".0 ","")+" "*10).substring(0,5)
    if ( (p< thisR.nbQbits) && (p != QReg.All)) { pad.nextCol(); pad.atAbs(p*2+1, angleStr); pad.previousCol() }
    pad.nextCol2();     pad.nextCol2()
    pad.nextCol2()
  }

  def idxBit: List[Int] = List(p);

  override  def op(qbit : QV):QV = { // Ry
    QM(
      QV(cos(o/2), -sin(o/2)),
      QV(sin(o/2), cos(o/2))
    ).mult(qbit)
  } // Ry
} // Ry

/**  Rotate Z */
case class Rz(p:Int = QReg.All, angle : Double) extends QOperator {

  var o = angle

  override def init(): Unit = {
    if ( ((p < 0) && (p != QReg.All)) ||
      (p >= thisR.nbQbits))  QReg.notifyError("in Rz("+p+",...) Incorrect #QBit")
    o = thisR.angle(angle)
  }

  override def opLabel = "Rz"+ (if (p == QReg.All) "" else "("+thisR.LabelOf(p) +","+angle +")")

  def render(pad: QPad): Unit = {
    pad.at(p, "Rz");
    val angleStr = ((angle.toString+" ").replaceAll(".0 ","")+" "*10).substring(0,5)
    if ( (p< thisR.nbQbits) && (p != QReg.All)) { pad.nextCol(); pad.atAbs(p*2+1, angleStr); pad.previousCol() }
    pad.nextCol2();     pad.nextCol2()
    pad.nextCol2()
  }

  def idxBit:  List[Int] = List(p);

  override   def op(qbit : QV):QV = { // Rz
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

/**  Conditionnal operator on list */
case class CL(Qop: QOperator, condList_ : List[Int]) extends QOperator {

  val condList: List[Int] = condList_.distinct

  override def init(): Unit = {
    condList.foreach ( p => if ( (p < 0) || (p >= thisR.nbQbits))  QReg.notifyError("in CL(...,"+p+") Incorrect control #QBit") )
  }

  override def opLabel = "CL( "+ Qop.opLabel +" , List("+ condList.map(idx => thisR.LabelOf(idx)).mkString(",") +") )"

  def render(pad: QPad): Unit = {
    val ll = idxBit ::: condList
    val mn = ll.min
    val mx = ll.max
    for(i <- mn*2 to mx*2) pad.atAbs(i,"│")
    for(i <- condList) pad.at(i,"•")
    Qop.render(pad)
  }
  def idxBit:  List[Int] = Qop.idxBit;
  override   def op( qbit : QV):QV = Qop.op(qbit)
} // C

/**  Conditionnal Operator */
case class C(Qop: QOperator, cond: Int) extends QOperator { // C as in CNOT -> C(Not(Idx), idxp) -> C (X(idx), idxp)

  override def init(): Unit = {
    if ( (cond < 0) || (cond >= thisR.nbQbits))  QReg.notifyError("in CL(...,"+cond+") Incorrect control #QBit")
  }

  override def opLabel = "C( "+ Qop.opLabel +" , "+ thisR.LabelOf(cond) +")"

  def render(pad: QPad): Unit = {
    val condList = List(cond)
    val ll = idxBit ::: condList
    val mn = ll.min
    val mx = ll.max
    for(i <- mn*2 to mx*2) pad.atAbs(i,"│")
    for(i <- condList) pad.at(i,"•")
    Qop.render(pad)
  }
  def idxBit:  List[Int] = Qop.idxBit;
  override   def op( qbit : QV):QV = Qop.op(qbit)
} // C



/**  Swap deux Qbitsr */
case class Swap(e1: Int, e2: Int) extends QOperator {

  override def init(): Unit = {
    if ((e1 == e2) || (e1 < 0) || (e2 < 0) ||
      (e1 >= thisR.nbQbits) || (e2 >= thisR.nbQbits)) QReg.notifyError("in Swap("+e1+","+e2+") Incorrect control #QBit")
  }

  override def opLabel = "Swap("+  thisR.LabelOf(e1) +","+  thisR.LabelOf(e2) +")"

  def render(pad: QPad): Unit = {
    if (e1 < e2) for(i <- e1*2 to e2*2) pad.atAbs(i,"│")
    else for(i <- e2*2 to e1*2) pad.atAbs(i,"│")
    pad.at(e1,"x")
    pad.at(e2,"x")
    pad.nextCol2()
  }
  def idxBit:  List[Int] = List(e1,e2);
  override   def op( qbit : QV):QV = Id.mult(qbit)
} // Swap


/** Measure along Z */
case class <(idx: Int = QReg.All) extends QOperator {

  override def init(): Unit = {
    if (((idx < 0) && (idx != QReg.All)) ||
      (idx >= thisR.nbQbits)) QReg.notifyError("in <("+idx+") Incorrect #QBit")
  }

  override def opLabel = "<" + (if (idx == QReg.All) "" else "("+thisR.LabelOf(idx) +")")

  def render(pad: QPad): Unit = {
    pad.DRenderAt(idx)
    pad.nextCol2(); pad.nextCol2();
    pad.toEnd(idx, ' '/* '═' */)
  }

  def idxBit = List(idx);

  override   def op(qbit: QV) : QV = Id.mult(qbit)
} // Label

/**  Function Operator */
case class F(name: String, fun: QReg => Unit, msg : String ="", expand : Boolean = false, skipTrace: Boolean = true ) extends QOperator {
  def render(pad: QPad): Unit = {
    pad.atAbs(0,"╓")
    for(i <- 1 to thisR.nbQbits*2) pad.atAbs(i,"║")
    thisR.infoline(name + (if (msg.length > 0) " : " else "" ) + msg)

    pad.atAbs((thisR.nbQbits-1)*2+2, "╙")
    pad.nextCol(); pad.nextCol2();

  }

  override def opLabel = "F( "+ name + " "+ msg +" )"

  def idxBit = List(0)

  override   def op(qbit: QV) : QV = Id.mult(qbit)
} // Label


// Alias based operators (mostly Functions)

/**  Quantum Fourier Transform */
case class QFT(name:String="", msg : String ="", expand : Boolean = false, skipTrace : Boolean = true) extends QOperator {
  override def alias = F(msg,  QOperator.qft, msg, expand = expand, skipTrace = skipTrace)
  def render(pad: QPad): Unit = {}
  override def opLabel = "QFT("+ name+" "+  msg +")"
  def idxBit = List()
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



