// VQS : Quantum Computing Simulation
// Gildas Ménier
// 2020
// gildas.menier@univ-ubs.fr
package Vqs.complex

import Vqs.complex.QComplex._

import Vqs.QUtils._

import scala.io.AnsiColor._
import scala.language.implicitConversions
import scala.math._

/** complex number
 *
 *  @constructor creates a Complex number
 *  @param re real value
 *  @param im imaginary value
 */

case class QComplex(val re: Double=0, val im: Double=0) {

  /** conjugate (lazy)*/
  lazy val conj: QComplex = QComplex(re, -im)

  /** squared norm (lazy)*/
  lazy val norm2: Double = re * re + im * im // norme au carré

  /** norm (lazy)*/
  lazy val norm : Double = sqrt(norm2)

  /** thetaP (lazy)*/
  lazy val thetaP : Double = atan2(im, re)



  /** (quantum) proba as norm (lazy)*/
  lazy val proba = { if (norm2 > 0.9999999999) 1.0 else norm2 }

  /** (quantum) bphase (lazy)*/
  lazy val bphase = {
    val (_, theta) = asEuler
    theta
  }

  /** (quantum) returns phase with an origin shift
   *  @param origin org shift (0.0 as default value)
   */
  def phase(origin : Double = 0.0) = {
    val v = normalizeAngleOrigin(bphase, origin)
    if (math.abs(v) < 0.00000001) 0.0 else v
  } // phase

  /** returns a new complex as sum of THIS and another complex
   *  @param z complex to add
   */
  def +(z: QComplex): QComplex = QComplex(re + z.re, im + z.im)

  /** returns a new complex as difference of THIS and another complex
   *  @param z complex for the difference
   */
  def -(z: QComplex): QComplex = QComplex(re - z.re, im - z.im)

  /** returns a new complex as product of THIS and a double
   *  @param x double to *
   */
  def *(x: Double): QComplex = QComplex(re * x, im * x)

  /** returns a new complex as negate (* -1)
   */
  def unary_- : QComplex = this * -1.0

  /** returns a new complex as product of THIS and another complex
   *  @param z complex to *
   */
  def *(z: QComplex): QComplex = QComplex(re * z.re - im * z.im, re * z.im + im * z.re)

  /** returns a new complex as div of THIS and a double
   *  @param z complex to div
   */
  def /(x: Double): QComplex = QComplex(re / x, im / x)

  /** returns a new complex as div of THIS and another complex
   *  @param z complex to div
   */
  def /(z: QComplex): QComplex = (this * z.conj) / (z * z.conj).re

  /** returns a new complex as rot
   *  @param thetac angle to rotate (in radians)
   */
  def rot(thetac: Double): QComplex = this * QComplex(math.cos(thetac), math.sin(thetac))

  val sSize = 17 // toString : Size of the ascii / graphics


  def asPhaseNormString(isRadian : Boolean, origin: Double = 0.0, ascii_ : Boolean = false): String = {
    val (r, thetap) = asEuler
    val rStr = formatNumber(r)

    // Finds and angle between 0 and 2Pi with Phase offset
    var ang_ = normalizeAngleOrigin(thetap, origin)
    var ang = (if (ang_ <= math.Pi) ang_ else math.Pi - ang_)

    val res = (new QComplex( r*math.cos(ang), r*math.sin(ang) )).toString

    if (abs(ang) < 0.0001) ang = 0

    res.replaceAll("0,70711","1/√2") // todo adds automatic symbolic reformating
  }

  /** returns a complex number as a string with Euler formatting
   *  @param origin phase shift (0.0 by default)
   */
  def asEulerString(isRadian : Boolean, origin: Double = 0.0, ascii_ : Boolean = false): String = { //  euler
    val (r, thetap) = asEuler
    val rStr = formatNumber(r)

    // Finds and angle between 0 and 2Pi with Phase offset
    var ang_ = normalizeAngleOrigin(thetap, origin)
    var ang = (if (ang_ <= math.Pi) ang_ else math.Pi - ang_)

    if (abs(ang) < 0.0001) ang = 0

    var angstr = formatAngle(isRadian, ang, ascii_)
    if (ang <0) angstr = angstr + " or "+ formatAngle(isRadian, ang + 2*math.Pi , ascii_)

    val res =
      if (abs((abs(r)-1.0))<0.0001) s"ei $angstr"
      else if (abs(r) < 1E-20) "0"
      else s"( $rStr .ei $angstr)"

    res.replaceAll("0,70711","1/√2") // todo adds automatic symbolic reformating
  } // asEulerString


  /** returns the complex as Euler
   */
  def asEuler:(Double, Double) = (norm, thetaP)

  /** (quantum) draws the complex as a string depicting a probability
   */
  def probaString(isEmpty : Boolean = false, ascii : Boolean = false):String = { // Drawing of proba
    var nb = (sSize*proba).toInt
    if ((proba >0)&&(nb==0))  nb = 1
    val res = if (proba < 0.1) {
      val nbP = (sSize*proba*10).toInt
      if (!ascii) {
        ("│" + ("█" * (nbP)) + "▒" * (sSize - nbP) + "│")
          .replaceAllLiterally("█", s"${MAGENTA}▓${RESET}")
          .replaceAllLiterally("│", s"${BLUE}│${RESET}")
      } else {
        ("|" + ("*" * (nbP)) + " " * (sSize - nbP) + "|")
          .replaceAllLiterally("*", s"${MAGENTA_B} ${RESET}")
          .replaceAllLiterally("│", s"${CYAN_B}│${RESET}")
      }

    } else {
      if (!ascii) {
        ("│" + ("█" * (nb)) + "▒" * (sSize - nb) + "│")
          .replaceAllLiterally("█", s"${BLUE}█${RESET}")
          .replaceAllLiterally("│", s"${BLUE}│${RESET}")
      } else {
        ("|" + ("*" * (nb)) + " " * (sSize - nb) + "|")
          .replaceAllLiterally("*", s"${CYAN_B} ${RESET}")
          .replaceAllLiterally("│", s"${CYAN_B}│${RESET}")
      }
    }
    val result = if (proba < 1E-20) res.replaceAllLiterally("▒", " ")  else res

    val p: String = (" "+proba+" "*6).substring(0,7)

    p +" " + (if (! isEmpty) result else { ("." + ("." * (sSize)) + ".") })
  } // probaString

  /** (quantum) draws the complex as a string depicting the phase
   *  @param origin shift for the phase
   *  */
  def phaseString(origin : Double, notEmpty : Int = 1, ascii : Boolean = false):String = {
    val (amplitude, phase_) = this.asEuler
    val miniA = if (Math.abs(amplitude) < 1E-10) true else false

    val phase = normalizeAngleOrigin(phase_, origin)

    val tabC =
      if (! ascii)
        if (notEmpty == 0) ("."+  ("."* sSize) +".").toCharArray else ("║"+  ("▒"* sSize) +"║").toCharArray
      else
        if (notEmpty == 0) ("."+  ("."* sSize) +".").toCharArray else ("["+  (" "* sSize) +"]").toCharArray


      if (notEmpty == 0) return tabC.mkString

    val p = (if (phase <= math.Pi) phase else math.Pi -phase)

    val ofs = ((p/math.Pi)*sSize/2+1).toInt
    var charS = ' '
    if (!ascii)
      if (miniA) charS = '▓' else charS = '█'
    else
      if (miniA) charS = ' ' else charS = '*'


    if (ofs >0) {
      for( i <- 0 until ofs) tabC(sSize/2+i+1) = charS
    } else if (ofs <0) {
      for( i <- ofs to 0) tabC(sSize/2+i) = charS
    }

    if ( math.abs((math.abs(p)-math.Pi)) < 0.00001 ) { // phase managing
      for(i <- 1 to sSize )
        if (! ascii)
          if (i<= sSize/2) tabC(i) = '▓' else tabC(i)=charS
        else
          if (i<= sSize/2) tabC(i) = ' ' else tabC(i)=charS
    }
    if (notEmpty==1)
      if (! ascii) tabC(sSize/2+1) = '█' else tabC(sSize/2+1) = '*'
    else
      if (! ascii) tabC(sSize/2+1) = '*' else tabC(sSize/2+1) = ' '


    if (!ascii) {
      if (!miniA)
        tabC.mkString.replaceAllLiterally(charS.toString, s"${YELLOW}$charS${RESET}").replaceAllLiterally("║", s"${YELLOW}║${RESET}")
      else
        tabC.mkString.replaceAllLiterally(charS.toString, s"${MAGENTA}$charS${RESET}").replaceAllLiterally("║", s"${YELLOW}║${RESET}")
    } else {
      if (!miniA)
        tabC.mkString.replaceAllLiterally(charS.toString, s"${YELLOW_B} ${RESET}").replaceAllLiterally("|", s"${YELLOW}|${RESET}")
      else
        tabC.mkString.replaceAllLiterally(charS.toString, s"${MAGENTA_B} ${RESET}").replaceAllLiterally("|", s"${YELLOW}|${RESET}")
    }


  } // ThetaString

  /** (quantum) String drawing with both probability and phase
   *  @param origin shift for the phase
   *  */
  def probaPhaseString(origin : Double, ascii_ : Boolean = false): String = {
    val proba: String = probaString()
    val phase: String = phaseString(origin)

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
  } // blends the two drawing


  /** returns a complex number with a phase correction
   *  @param origin phase shift (0.0 by default, radian)
   */
  def phaseUnShift(origin: Double = 0.0): QComplex = { //
    val (r, thetap) = asEuler

    // Finds and angle between 0 and 2Pi with Phase offset
    var ang_ = normalizeAngleOrigin(thetap, origin)
    var ang = (if (ang_ <= math.Pi) ang_ else math.Pi - ang_)

    if (abs(ang) < 0.0001) ang = 0

    new QComplex(r*math.cos(ang), r*math.sin(ang))
  } // asEulerString



  /** Complex as a 'a+ib' string */

  override def toString: String = {
    val reStr = formatNumber(re)
    val imStr = formatNumber(im) +"i"
    if (math.abs(im) < 0.00001) " "+reStr
    else if (math.abs(re) < 0.00001) s"($imStr)"
    else s"($reStr+ $imStr)"
  } // toString

} // QComplex



/** Factory for mini Complex DSL */

object QComplex {


  /** Implicit Creates a complex from i
   *
   *  @param x
   */
  implicit def toImaginary(x: Double) = new {
    def i = new QComplex(0.0, x)
  }

  /** Implicit Creates a complex from i
   *
   *  @param x
   */
  implicit def toComplex(x: Double) = new QComplex(x, 0.0)

  /** Creates a complex from Euler  r.ei(theta)
   *
   *  @param r norm
   *  @param theta angle
   */
  def ComplexEuler(r: Double, theta : Double) = QComplex(r*cos(theta), r*sin(theta))

  /** Constant i */
  val i = new QComplex(0.0, 1.0)

  /** Constant 1 */
  val one = new QComplex(1.0, 0.0)

  /** Constant 0 */
  val zero = new QComplex(0.0, 0.0)

  /** returns a new complex as sum of two complex
   *  @param x
   *  @param y
   */
  def plus(x: QComplex, y: QComplex) = x + y

  /** returns a new complex as product of two complex
   *  @param x
   *  @param y
   */
  def mult(x: QComplex, y: QComplex) = x * y

  lazy val df = new java.text.DecimalFormat("#.##### ")

  /** converts a number n to a string
   *  @param n number to format
   */
  def formatNumber(n: Double) : String = {
    var res = df.format(n)
    res = if (res(0) != '-') " "+res else res
    res.replaceAll("0,70711","1/v2") // todo 1/2
  } // formatNumber

  /** converts an angle ang to a string
   *  @param ang angle to format
   */
  def formatAngle(isRadian : Boolean, ang : Double, ascii : Boolean = false) : String = {
    var strAng = ""

    if (isRadian) {
      val thetapStr = df.format(ang / Pi)

      strAng = s"$thetapStr" + " π"
      if (thetapStr.length > 2) {
        if (thetapStr.substring(0, 2) == "1 ") if (!ascii) strAng = "π" else strAng = "P"
        if (thetapStr.substring(0, 3) == "-1 ") if (!ascii) strAng = "π" else strAng = "-P"
      }
      if (math.abs(ang / Pi) < 0.00001) strAng = "0"
    } else {
      val cvt = cvtRadToDeg(ang)
      val df2 = if (!ascii) new java.text.DecimalFormat("##.##°") else new java.text.DecimalFormat("##.##")
      val thetapStr = df2.format(cvt)
      strAng = thetapStr.replaceAll("-0°","0°")
    }

    strAng
  } // formatAngle


} // QComplex


