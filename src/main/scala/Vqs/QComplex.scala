// VQS : Quantum Computing Simulation
// Gildas Ménier
// 2020
// gildas.menier@univ-ubs.fr
package Vqs


import scala.math._

import QComplex._
import io.AnsiColor._


case class QComplex(val re: Double, val im: Double) {

  val sSize = 17 // Size of the ascii / graphics

  lazy val conj: QComplex = QComplex(re, -im)
  lazy val norm2: Double = re * re + im * im // norme au carré
  lazy val proba = { if (norm2 > 0.9999999999) 1.0 else norm2 }
  lazy val bphase = {
    val (_, theta) = asEuler
    theta
  }
  lazy val norm : Double = sqrt(norm2)
  lazy val module : Double = norm

  lazy val thetaP : Double = atan2(im, re)

  def phase(origine : Double = 0.0) = {
    val v = normalizeAngleOrigin(bphase, origine)
    if (math.abs(v) < 0.00000001) 0.0 else v
  } // phase

  def +(z: QComplex): QComplex = QComplex(re + z.re, im + z.im)
  def -(z: QComplex): QComplex = QComplex(re - z.re, im - z.im)
  def *(x: Double): QComplex = QComplex(re * x, im * x)
  def unary_- : QComplex = this * -1.0
  def *(z: QComplex): QComplex = QComplex(re * z.re - im * z.im, re * z.im + im * z.re)

  def /(x: Double): QComplex = QComplex(re / x, im / x)
  def /(z: QComplex): QComplex = (this * z.conj) / (z * z.conj).re
  def rot(thetac: Double): QComplex = this * QComplex(math.cos(thetac), math.sin(thetac))

  private lazy val df = new java.text.DecimalFormat("#.########  ")

  def asEulerString(start: Double = 0.0): String = { //  euler
    val (r, thetap) = asEuler
    // val rStr = df.format(r)
    val rStr = r.toString()

    // Finds and angle between 0 and 2Pi with Phase offset
    var ang_ = normalizeAngleOrigin(thetap, start)
    val ang = (if (ang_ <= math.Pi) ang_ else math.Pi - ang_)

    val angstr = formatAngle(ang)

    val res =
      if (abs((abs(r)-1.0))<0.0001) s"ei $angstr"
      else if (abs(r) < 1E-20) "0"
      else s"(| $rStr| ei $angstr)"

    res.replaceAll("0,70711","1/√2")
  } // asEulerString

  def asEuler:(Double, Double) = (norm, thetaP)

  def probaString:String = { // Drawing of proba
    var nb = (sSize*proba).toInt
    if ((proba >0)&&(nb==0))  nb = 1
    val res = if (proba < 1E-10) {
      ("│" + ("█" * (nb)) + "▒" * (sSize - nb) + "│")
        .replaceAllLiterally("█", s"${MAGENTA}▓${RESET}")
        .replaceAllLiterally("│", s"${BLUE}│${RESET}")
    } else {
      ("│" + ("█" * (nb)) + "▒" * (sSize - nb) + "│")
        .replaceAllLiterally("█", s"${BLUE}█${RESET}")
        .replaceAllLiterally("│", s"${BLUE}│${RESET}")
    }
    if (proba < 1E-20) res.replaceAllLiterally("▒", " ")
    else res
  } // probaString

  def phaseString(offset : Double):String = {
    val (amplitude, phase_) = this.asEuler
    val miniA = if (Math.abs(amplitude) < 1E-10) true else false

    val phase = normalizeAngleOrigin(phase_, offset)

    val tabC = ("║"+  ("▒"* sSize) +"║").toCharArray
    val p = (if (phase <= math.Pi) phase else math.Pi -phase)

    val ofs = ((p/math.Pi)*sSize/2+1).toInt
    var charS = ' '
    if (miniA) charS = '▓' else charS = '█'

    if (ofs >0) {
      for( i <- 0 until ofs) tabC(sSize/2+i+1) = charS
    } else if (ofs <0) {
      for( i <- ofs until 0) tabC(sSize/2+i+1) = charS
    } else {

    }

    if ( math.abs((math.abs(p)-math.Pi)) < 0.00001 ) { // phase managing
      for(i <- 1 until sSize-1 )
        if (i<= sSize/2) tabC(i) = '▓' else tabC(i)=charS
    }
    tabC(sSize/2+1) = '║'
    if (! miniA)
      tabC.mkString.replaceAllLiterally(charS.toString, s"${YELLOW}$charS${RESET}").replaceAllLiterally("║", s"${YELLOW}║${RESET}")
    else
      tabC.mkString.replaceAllLiterally(charS.toString, s"${MAGENTA}$charS${RESET}").replaceAllLiterally("║", s"${YELLOW}║${RESET}")
  } // ThetaString

  def probaPhaseString(offset : Double): String = {
    val proba: String = probaString
    val phase: String = phaseString(offset)

    val res = ( " " * (proba.length)).toCharArray

    for(i <- 0 until res.length) {
      proba(i) match {
        case '│' => res(i)='║'
        case '░' => {
          phase(i) match {
            case '░' =>  res(i) = '░'
            case '▓' =>  res(i) = '▄'
            case '║' => res(i) = '║'
          }
        }
        case '▓' => {
          phase(i) match {
            case '░' =>  res(i) ='▀'
            case '▓' =>  res(i) ='█'
            case '║' => res(i) = '║'
          }
        }
      }
    }

    res.mkString
  } // mix the two drawing

  override def toString: String = {
    val reStr = formatNumber(re)
    val imStr = formatNumber(im) +"i"
    if (math.abs(im) < 0.00001) reStr
    else if (math.abs(re) < 0.00001) s"($imStr)"
    else s"($reStr+ $imStr)"
  } // toString

} // QComplex



object QComplex {

  var isRadian = true;

  implicit def toImaginary(x: Double) = new {
    def i = new QComplex(0.0, x)
  }
  implicit def toComplex(x: Double) = new QComplex(x, 0.0)

  def ComplexEuler(r: Double, theta : Double) = QComplex(r*cos(theta), r*sin(theta))

  val i = new QComplex(0.0, 1.0)
  val one = new QComplex(1.0, 0.0)
  val zero = new QComplex(0.0, 0.0)

  def plus(x: QComplex, y: QComplex) = x + y
  def mult(x: QComplex, y: QComplex) = x * y

  def formatNumber(n: Double) : String = {
    val df = new java.text.DecimalFormat("#.##### ")
    var res = df.format(n)
    res = if (res(0) != '-') " "+res else res
    res.replaceAll("0,70711","1/√2")
  } // formatNumber


  def formatAngle(ang : Double) : String = {
    var strAng = ""

    if (QComplex.isRadian) {
      val df = new java.text.DecimalFormat("#.##### ")
      val thetapStr = df.format(ang / Pi)

      strAng = s"$thetapStr" + " π"
      if (thetapStr.length > 2) {
        if (thetapStr.substring(0, 2) == "1 ") strAng = "π"
        if (thetapStr.substring(0, 3) == "-1 ") strAng = "-π"
      }
      if (math.abs(ang / Pi) < 0.00001) strAng = "0"
    } else {
      val cvt = convertRadToDec(ang)
      val df = new java.text.DecimalFormat("##°")
      val thetapStr = df.format(cvt)
      strAng = thetapStr.replaceAll("-0°","0°")
    }

    strAng
  } // formatAngle


  def convertDecimalToFraction(x: Double): (Int, Int) = {
    if (x < 0) {
      val r = convertDecimalToFraction(-x)
      (-r._1, r._2)
    } else {
      val tolerance = 1.0E-6
      var h1 = 1.0; var h2 = 0.0; var k1 = 0.0; var k2 = 1.0; var b = x
      do {
        val a = Math.floor(b); var aux = h1;
        h1 = a * h1 + h2; h2 = aux;
        aux = k1; k1 = a * k1 + k2;
        k2 = aux; b = 1 / (b - a);
      } while ( {
        Math.abs(x - h1 / k1) > x * tolerance
      })
      (h1.toInt,k1.toInt)
    }
  } // convertDecimalToFraction


  def normalizeAngleOrigin(ang_ : Double, start: Double = 0.0): Double = {
    var ang = (ang_ - start)+100*math.Pi
    while (math.abs(ang) >= 2*math.Pi-0.00001) ang = ang - 2*math.Pi;
    ang
  } // normalizeAngleOrigin

  def convertDecToRad(v: Double) = (v*Math.PI)/180
  def convertRadToDec(v: Double) = (v*180)/Math.PI


} // QComplexe


