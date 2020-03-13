package Vqs
// QSim : Simulateur Quantique
// Gildas Ménier
// 2020
// gildas.menier@univ-ubs.fr


import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.math.abs

import QReg._

import io.AnsiColor._

class QPad(nbQbits : Int, Qreg_ : QReg) { // graphical pad

  val lggrille: Int = 2000
  var currentValue: Int = 0

  var myQReg = Qreg_

  val mem: mutable.ArrayBuffer[QOperator] = mutable.ArrayBuffer[QOperator]()
  val show : mutable.ArrayBuffer[Boolean] = mutable.ArrayBuffer[Boolean]()

  val screen: Array[Array[String]] = Array.ofDim[String](nbQbits*2+2, lggrille)
  for( i <- 0 until nbQbits*2+2; j <- 0 until lggrille ) screen(i)(j)   = " "
  for( i <- 0 until nbQbits  ; j <- 0 until lggrille ) screen(i*2)(j) = "—"


  var colonne: Int = 0

  writeValue(0)

  def getState(idx:  Int): Int = {
    myQReg.getState(idx)
  }

  def DRenderAt(idx: Int): Unit = {
    if (idx != All) at(idx, "<(" + this.getState(idx) + ")  ")
    else {
      (0 until nbQbits).foreach( v => DRenderAt(v))
    }
  }

  def at(p : Int, s: String): Unit = {
    if (p==QReg.All) { // tous les QBits
      (0 until nbQbits).foreach( v => at(v, s))
    } else atAbs(p*2, s)
  }


  def infol(s : String): Unit = { // info dans les lignes sous le graphe
     atAbs((nbQbits-1)*2+1, (s+" "*5).substring(0,5))
  }


  def infoline(s : String): Unit = { // info dans les lignes sous le graphe
    atAbs((nbQbits-1)*2+2, s)
  }

  def label(s: String): Unit = {
    atAbs((nbQbits/2)*2-1, s)
  } // label

  def atAbs(p: Int, s:String) = {
    s.indices.foreach( v =>
      if ( ((colonne+v) < lggrille-2) && (p < nbQbits*2+2) )screen(p)(colonne+v) = s(v).toString
    )
  } // atAbs


  def vlabel(s:String) = {
    s.indices.foreach( v =>
      screen(v)(colonne) = s(v).toString
    )
  } // vlabel


  def add(QP : QOperator, show_ : Boolean) { this.mem += QP; this.show += show_ }

  def writeValue(v: Int): Unit =  {
    currentValue = v
    colonne = 0
    (0 until nbQbits).foreach(
      idx => at(idx, "  "+idx+": ")
    )
    for (i <- 0 until 5) nextCol()
    val bina = QUtils.toBinary(v, nbQbits)
    bina.indices.foreach(
      idx => at(idx, "%"+(bina(nbQbits-idx-1).toString))
    )
    nextCol(); nextCol()
    bina.indices.foreach(
      idx => at(idx, ">")
    )
    nextCol();
    nextCol()
  } // writeValue

  def toEnd(idx: Int, ch : Char): Unit = {
    if (idx != All) {
      for (i <- colonne until lggrille) screen(idx * 2)(i) = ch.toString
    } else {
      (0 until nbQbits).foreach(v => toEnd(v, ch))
    }
  }

  def pushNextCol( offset : Int): Unit = {
    colonne = colonne+offset // prochaine colonne
  } // pushNextCol

  def nextCol(): Unit = {
    colonne = colonne+1
  } // nextCol

  def previousCol(): Unit = {
    colonne = colonne-1
  } // nextCol

  def nextCol2(): Unit = {
    colonne = colonne+2
  } // nextCol2

  def previousCol2(): Unit = {
    colonne = colonne-2
  }

  def getCol: Int = colonne;

  def render()= {

    colonne = 9

    mem.indices.foreach(
      idx => if (show(idx)) mem(idx).render(this)
    )

    var last = lggrille-2
    while ((last >0) && ((screen((nbQbits-1)*2+1)(last) == " ") && (screen((nbQbits-1)*2+2)(last) == " "))) last = last -1
    if (last >0) colonne = math.max(last+1, colonne)

    "\n QBit\n  #  v\n"+(for( i <- 0 until nbQbits*2+1) yield {

        (for (j <- 0 until colonne) yield screen(i)(j)(0).toString).mkString

    }).mkString("\n").replaceAllLiterally("—", s"${BLUE}—${RESET}")
      .replaceAllLiterally(">", s" ${BLUE}>${RESET}")
      .replaceAllLiterally("<", s"${BOLD}${BLUE}<${RESET}")

  } // render




}
