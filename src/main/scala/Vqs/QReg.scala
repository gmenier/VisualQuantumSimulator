// VQS : Quantum Computing Simulation
// Gildas Ménier
// 2020
// gildas.menier@univ-ubs.fr
package Vqs

import java.awt.geom.Ellipse2D
import java.awt.image.BufferedImage
import java.awt.{Color, Image}
import java.io.File
import java.security.SecureRandom

import complex._
import operators._
import output._
import QReg._
import QUtils._

import io.AnsiColor._

/** Quantum Register
 *
 *  @constructor creates a Quantum register
 *  @param nbQbits size of the register (in QBits)
 */
case class QReg(val nbQbits : Int = 1) { //

  // how many different values
  val nbValues : Int = scala.math.pow(2, nbQbits).toInt

  // Array of states (each initialized to 0+0i)
  val state: Array[QComplex] = Array.fill[QComplex](nbValues)(QComplex(0,0))

  // to keep track of the states that may have changed
  val changed: Array[Boolean] = Array.fill[Boolean](nbValues)(false)

  // to keep track of the QBits that have changed
  val qbitChanged: Array[Boolean] = Array.fill[Boolean](nbQbits)(false)

  // by default, the first QBit is 1, so the only state if 0 with a full probability
  this.state(0) = QComplex(1,0) // valeur 000 par défaut

  val NOPURESTATE = -1
  // a pure state is 0 or 1 (after measure)
  val qbMstate = Array.fill[Int](nbQbits)(NOPURESTATE) // nostate, since no measure yet


  // by default, gets the value from Companion Object
  var isInRadians = QReg.DefaultObjIsInRadians // true if this reg is in radian

  def setRadians() { this.isInRadians = true }
  def setDegrees() { this.isInRadians = false }

  var isTrace = false // no trace yet

  var traceIdx = 0
  var traceSize = 0

  val circuitSize = 30

  var isShowRender : Boolean = true // renders or not

  var drawAllState : Boolean = QReg.DefaultDrawAllState; // if false only draw states with probability > 0

  var lastOp : String ="" // last operator used (trace)

  var phaseNormalization : Boolean = QReg.DefaultObjPhaseNormalization // if true normalize the phases

  var renderConsoleIDEA = ! QReg.DefaultObjUseASCII
  var onlyAscii = QReg.DefaultObjUseASCII

  def useOnlyASCII(v : Boolean): Unit = {
    this.renderConsoleIDEA = ! v
    this.onlyAscii = v
  }


  // Resets the system that keeps track of the changed Qbits and values
  def resetChange(): Unit = {
    changed.indices.foreach( v => changed(v) = false)
    qbitChanged.indices.foreach( v => qbitChanged(v) = false)
  }

  /** State : starts the trace */
  def traceOn() { this.isTrace = true }
  /** State : stops the trace */
  def traceOff() { this.isTrace = false }

  /** State : no rendering (during trace) */
  def hideRender() { this.isShowRender = false }
  /** State : rendering (during trace) */
  def showRender() { this.isShowRender = true }

  // The object used to output pdf
  var myPdf: PdfReport = null

  // builds the pdf object used
  def pdf(fn : String, docLabel: String=""): Unit = {
    this.myPdf = new PdfReport(fn, docLabel)
    this.myPdf.newPage()
  }



  /** Begins the trace */
  def trace(sizeOfTrace : Int = 2, useASCII : Boolean = this.onlyAscii): Unit = {
    this.useOnlyASCII(useASCII)
    QUtils.createImagesDirectoryIfNecessary
    QUtils.removeImages
    isTrace = true
    traceIdx = 0
    traceSize = sizeOfTrace
    println("\n>VQS: Starting trace "+ (if (useASCII) " with ASCII output" else "") )
    println(if (this.renderConsoleIDEA) this.render else this.renderWithoutAnsiClean)
    println(this)
    this.resetChange()
    this.drawStateImage(filename = "trace_"+traceIdx, numLines = traceSize, text="Starting trace")
    if (this.myPdf != null) { // création d'un fichier pdf
      this.myPdf.writeReport(this, circuitSize, traceIdx)
    }
    traceIdx = traceIdx+1
  }


  /** State : the drawing displays the normalized phase */
  def drawPhaseNormalization(): Unit = {
    phaseNormalization = true
  }

  /** State : the drawing does not display the normalized phase */
  def drawNOPhaseNormalization(): Unit = {
    phaseNormalization = false
  }


  // is an ascii graphic pad to display the circuit
  val pad = new QPad(nbQbits, this)




  // get the states
  /** Gets the state for a reg value (non pure)
   *  @param idx_ of the value
   *  @return 0, 1 or -1 (not measured)
   */
  def apply(idx : Int): QComplex = this.state(idx)

  /** Gets the state for a reg value (non pure) in binary
   *  @param idx_ of the value (binary)
   *  @return 0, 1 or -1 (not measured)
   */
  def apply(idx : String) : QComplex = this(QUtils.binaryToInt(idx))




  /** Gets the measured state for a reg value (pure)
   *  @param idx_ of the value
   *  @return 0, 1 or -1 (not measured)
   */
  def getMState(idx : Int) = {
    qbMstate(idx)
  }


  /** Gets the measured state for a reg value (pure) - raises an internal error
   *  @param idx_ of the value
   *  @return 0, 1 or -1 (not measured)
   */
  def readMQbit(idx: Int) = { // reads a QBit value - should < before reading
    val r = getMState(idx)
    if (r == -1) {
      notifyError(s"Trying to read the Qbit #${idx} not fixed yet (try <())")
      0
    } else {
      r
    }
  } // readQbit


  /** Gets the measured state for the full register (should < before)
   */
  def getMQbit: Int = { // Read the reg value - should < before
    val rl : List[Int] = (for (i <- 0 until nbQbits) yield readMQbit(i)).toList
    rl.reverse.foldLeft(0)(
      (ac,v) => v + 2*ac
    )
  } // readQbit

  /** Sets the measured state for a value in the register
   */
  def setMState(idx: Int, value: Int) { qbMstate(idx) = value }





  /** State : Only draws values if state has a non null amplitude */
  def drawOnlyPossible(): Unit = { // only draws proba >0
    this.drawAllState = false
  } // drawOnlyPossible

  /** State : draws all (even if null) */
  def drawAll(): Unit = { /// draws ALL states
    this.drawAllState = true;
  } // drawAll



  // Changes the state and tracks the change
  def update(idx : Int, value: QComplex) {
    this.state(idx) = value
    this.changed(idx) = true
  } // update

  // updates with binary idx
  def update(idx : String, value: QComplex) {
    this.update(QUtils.binaryToInt(idx), value)
  } // update



  // writes a value in the reg
  def write(value : Int) { // initialise the first register's value
    // TODO qbitchanged
    this.state.indices.foreach( v => this(v)= QComplex(0,0))
    this(value) = QComplex(1,0)
    pad.writeValue(value) // valeur courante
  } // write


  /** inner OP : Write a value in the Reg. Should be done at the start only */
  def init(value: Int = 0): Unit = {
    lastOp = "init("+value+")"
    qbitChanged.indices.foreach( i => qbitChanged(i)=true )
    this.write(value)
    processTraceIfNecessary()
  }



  /** inner OP : Performs a measure on a value or of all is QReg.All */
  def forceRead(idx: Int) { // Force the reading of a QBit

    if (idx == QReg.All) {
      (0 until nbQbits).foreach( v => forceRead(v))
    } else {
      qbitChanged(idx) = true;

      val res = (0 until math.pow(2, nbQbits).toInt).map(v => (v, QUtils.toBinary(v, nbQbits))).groupBy(v =>
        v._2(nbQbits - 1 - idx)
      )

      // Sum of all proba

      var proba0 = res('0').foldLeft(0.0)((a, v) => a + this (v._1).proba)
      // val proba1 = res('1').foldLeft(0.0)((a, v) => a + this(v._1).proba)


      val f = flip(proba0) // tries to get a 0

      if (f) { // a 0
        res('1').foreach(v => this (v._1) = QComplex(0, 0)) // cancels probabilities for 1
      } else { // a 1
        res('0').foreach(v => this (v._1) = QComplex(0, 0)) // cancels probabilities for 0
      }

      normalize()
      val vf = if (f) 0 else 1
      setMState(idx, vf)
    }
  } // read


// displays a string on the pad's infoline
  def infoline(s: String): Unit = {
    this.pad.infoline("  "+s)
  } //infoline


// main op invocation system
  // - is the wire and the qop_p is applied on the current register

  def -(qop_p : QOperator): QReg =  {

    var qop_ = qop_p.alias
    this.resetChange() // keeps a trace of the states

    var condl = List[Int]() // Conditionnal QBit list

      qop_.setRegister(this)
      qop_.init()
      var qop: QOperator = qop_
      pad.add(qop, isShowRender)

      qop match {

        case <(idx)     => {lastOp = "< (measure)"; forceRead(idx)}

        case F(_, fct, _, expand, skipTrace) =>
          var svgTrace = isTrace
          if (skipTrace) this.traceOff(); if (!expand) this.hideRender()
          fct(this);
          if (!expand) this.showRender(); this.isTrace = svgTrace
          this - VLabel("╖"+"║"*((nbQbits)*2-3)+"╜")

        case Label(_)   =>
        case VLabel(_)  =>
        case |(_)       =>

        case Swap(e1, e2) =>
          val r = (0 until math.pow(2, nbQbits).toInt).
            map(v => QUtils.toBinary(v, nbQbits)).
            filter(v => v(nbQbits - e1 - 1) != v(nbQbits - e2 - 1)).
            groupBy(v => {
              val a: Array[Char] = v.toCharArray;
              a(nbQbits - e1 - 1) = '*';
              a(nbQbits - e2 - 1) = '*';
              a.mkString
            }).
            foreach(
              v => {
                val s1 = QUtils.binaryToInt(v._2(0));
                val s2 = QUtils.binaryToInt(v._2(1))
                val tempC = this (s2);
                this (s2) = this (s1);
                this (s1) = tempC // swap
                qbitChanged(e1) = true
                qbitChanged(e2) = true
              }
            ) // foreach

        case _ => {

          var masque: Int = -1

          qop match {
            case CL(nqop, cond) => // Conditionnal
              masque = cond.foldLeft(0)((m, v) => m + (math.pow(2, v)).toInt)
              qop = nqop
              qop.setRegister(this)
              qop.init()
              condl = cond

            case C(nqop, condi) => // Conditionnal
              val cond = List(condi)
              masque = cond.foldLeft(0)((m, v) => m + (math.pow(2, v)).toInt)
              qop = nqop
              qop.setRegister(this)
              qop.init()
              condl = cond
            case _ =>
          }

          var idxQBit = qop.idxBit

          if (idxQBit >= nbQbits) notifyError("Accessing a non existent Qbit (check the # in the operator)")

          if (idxQBit == All) { // Multiple QBits (all QBits)
            (0 until nbQbits). foreach( v => applyOp(v, masque, qop) )
          } else {
            if (getMState(idxQBit) != -1) {
              notifyError("QBit "+idxQBit+" is flat")
            } else {
              applyOp(idxQBit, masque, qop)
            }
          }

        }

      }

      if (qop_.leaveATrace) processTraceIfNecessary(condl)

    this
  }  // ~

  // utility to perform trace if the state(s) allows it and when the state(s) allows it
  def processTraceIfNecessary(condl : List[Int] = List()): Unit = {
    if (isTrace) {
      println("_"*60+"\n")
      println(" Step("+traceIdx+ ") after "+lastOp)
      println(if (this.renderConsoleIDEA) this.render else this.renderWithoutAnsiClean)
      println(this)
      this.drawStateImage(filename = "trace_"+traceIdx, numLines = traceSize, text="("+traceIdx+ ") after "+lastOp, clist= condl)
      if (this.myPdf != null) { // Creates a pdf file
          this.myPdf.writeReport(this, circuitSize, traceIdx)
      }
      traceIdx = traceIdx + 1
    }
  }

// performs an op
  def applyOp(idxQBit : Int, mask : Int, qop : QOperator ) {
    // applies the qop on the Qbit idxQBit
    // conditionnal to the idxQbit in  the mask (if mask >0)

      qbitChanged(idxQBit) = true // tags this Qbit as changed

      lastOp = qop.opLabel // last op applied (this one)

      // Computes the couple of Qbits with only one V different
      val p = math.pow(2, idxQBit).toInt // 2^idxQbit
      val s = (0 until nbValues).groupBy(_ / p).toList.sortBy(_._1).groupBy(_._1 % 2 == 0)
      val rp = s(true).flatMap(c => c._2.toList)
      val ri = s(false).flatMap(c => c._2.toList)
      var vr = rp.zip(ri)

      if (mask > 0) vr = vr.filter(v => (v._1 & mask) > 0)

      // Applies the op on these states
      var f: QV => QV = qop.op _

      vr.foreach {
        case (i1, i2) =>
          val v = f(QV(this (i1), this (i2))) // matrix *
          this (i1) = v(0);
          this (i2) = v(1)
      }
    } // applyOp


  // performs a normalization on the Qreg
  def normalize() { // Born rule
    val s = math.sqrt(this.state.foldLeft(0.0)( (a,c) => a+c.norm2))
    this.state.indices.foreach(
      i => this.state(i) = this.state(i)/s
    )
  } // normalize

  // generates a drawing in an ASCII colored ANSI code list
  def render : String = {
    var r = pad.render()

    val colorBits = Array( RED, GREEN, MAGENTA, YELLOW,  CYAN )

    for (i <- 0 until nbQbits) {
        val nbs = i+":"
        val c = colorBits(i % colorBits.length)
        r = r.replaceAllLiterally(nbs, s"${c}${nbs}${RESET}")
    }

    r.replaceAllLiterally("(0)", s" ${BOLD}0${RESET} ")
      .replaceAllLiterally("(1)", s" ${BOLD}1${RESET} ")
      .replaceAllLiterally("%0", s"${BOLD}0")
      .replaceAllLiterally("%1", s"${BOLD}1")
      .replaceAllLiterally("╓", s"${YELLOW}╓${RESET}")
      .replaceAllLiterally("║", s"${YELLOW}║${RESET}")
      .replaceAllLiterally("╙", s"${YELLOW}╙${RESET}")
      .replaceAllLiterally("╖", s"${YELLOW}╖${RESET}")
      .replaceAllLiterally("╜", s"${YELLOW}╜${RESET}")  +"\n"
  }


  // renders without ANSI
  // handy for pdf generation for instance
  def renderWithoutAnsi : String = {
    cutRenderWithoutAnsi(2000)
  }

  def renderWithoutAnsiClean : String = {
    cutRenderWithoutAnsi(2000, "|", lg="-", llg="-")
  }

  def cutRenderWithoutAnsi(dSize : Int, bv: String ="|", lg : String ="-", llg : String ="—") : String = {
    var rend = this.render
      .replaceAll("\\x1B...?m", "")
      .replaceAll("│", bv)
      .replaceAll("—", llg)
      .replaceAll("║", bv).replaceAll("╓", bv)
      .replaceAll("╜", bv).replaceAll("╖", bv).replaceAll("╙",lg)
    val maxSize = rend.split("\n").map(_.length).max
    // rend has been stripped from ANSI Chars
    var from = 10+ maxSize-dSize
    if (dSize >= maxSize) from = -1
    if (from == -1) rend
    else {
      var lpos = 0;
      (for (line <- rend.split("\n")) yield ({
        lpos = lpos + 1
        if (line.length > 10) {
          if ((lpos > 1) && (lpos % 2 == 0) && (lpos < (nbQbits + 2) * 2))
            line.substring(0, 9) + "..." + line.substring(from, line.length - 1)
          else
            line.substring(0, 9) + "   " + line.substring(from, line.length - 1)
        } else line
      })).mkString("\n")
    }
  } // cutRender


  /** outputs a string with the current state of the register
    */
  override def toString : String = {

    var startAng : Double = 0.0

    val titrePhase =
      if (! this.onlyAscii)
               if (this.isInRadians) "Phase [-π 0 π]     " else "[-180°   0    180°]"
      else     if (this.isInRadians) "Phase [-Pi 0 Pi] "   else "[-180    0    180 ]"

    if (phaseNormalization)
        startAng = findPhaseOrg  // Normalizes

    var elt : List[Int] = (0 until nbValues).toList

    if (!this.drawAllState) elt = elt.filter( n => Math.abs(this(n).asEuler._1) > 1E-10 )

    var title = if (! this.onlyAscii)
      "Proba [0 -> 1]"+" "*6+titrePhase+" "*5+ "V\t    Bin\t\t\t   α\t\t\t\t\t\t\t\t|r|ei Θ"
    else
      "Proba [0 -> 1]"+" "*6+titrePhase+" "*9+ "V\tBin\t\t   a\t\t\t\t        |r|ei Theta"

    var res = title +"\n"+
      elt.map(v => this(v).probaString(isEmpty = (this(v).norm < QReg.MinNorm), ascii = this.onlyAscii)+" "+
        { if (this(v).norm < QReg.MinNorm) this(v).phaseString(startAng,0, ascii = this.onlyAscii)
        else                               this(v).phaseString(startAng, ascii = this.onlyAscii)
        } +
        "\t\t"+
        (v.toString+"     ").substring(0,5)+
                              "\t|"+(QUtils.toBinary(v,nbQbits)+">\t"+" "*10).substring(0,12) +
        {
          if (this(v).norm < QReg.MinNorm)  "  ."+" "*29 else (this(v).toString+" "*30).substring(0,30)
        } + {
        (if (this(v).norm < QReg.MinNorm) "\t= ." else "\t\t= "+this(v).asEulerString(this.isInRadians,startAng, this.onlyAscii))
        }
    ).mkString("\n")

    res = QUtils.colorizeBinary(nbQbits, res)
    res+"\n"
  } // toString


  // looks for a phase origin
  def findPhaseOrg: Double = { // finds the phase reference
    var it = 0;
    while ((it < nbValues) && ( math.abs(this(it).proba)) < 0.000000001) it = it+1
    if ( it < nbValues) this(it).phase() else this(0).phase()
  }


  // convert an angle to Deg if needed
  def angle(a :Double) = { // converts angle - all the computations are in Radians
      if (this.isInRadians) a else convertDegToRad(a)
  }


  // creates an image and saves it
  def drawCircleImage(filename : String ="registre", zoom : Double = 1.0, text: String = ""): Unit = {
    val im = GraphCanvas()
    var svg = this.drawAllState

    drawOnlyPossible()
    var offset:Double = 0.0
    if (phaseNormalization)
      offset = findPhaseOrg // normalizes

    var elt : List[Int] = (0 until nbValues).toList

    if (!this.drawAllState) elt = elt.filter( n => Math.abs(this(n).asEuler._1) > 1E-10 )

    im.drawFilledCircle(0,0,500, new Color(0,0,200))
    im.drawFilledCircle(25,25,500-50, new Color(0,200,250))

    for( i <- ((25 until 250) by 10).reverse)
      im.drawFilledCircle(250-i,250-i,i*2, new Color(0,0,i))

    im.drawFilledCircle(250-4,250-4,4*2, new Color(241,192,13))
   // im.drawFilledCircle(250-25,250-25,50, new Color(0,0,0))

    im.drawCircle((250-250*zoom/2).toInt,(250-250*zoom/2).toInt, (2*250*zoom*0.5).toInt, new Color(0,0,250))
    im.drawCircle((250-250*zoom/4).toInt,(250-250*zoom/4).toInt, (2*250*zoom/4).toInt, new Color(0,0,0))
    im.drawCircle((250-3*250*zoom/4).toInt,(250-3*250*zoom/4).toInt, (3*2*250*zoom/4).toInt, new Color(0,100,180))

    im.drawText("1/2", (250-250*zoom/2).toInt+40,(250-250*zoom/2).toInt+40, c= new Color(180,180,180))
    im.drawText("3/4", (250-3*250*zoom/4).toInt+40,(250-3*250*zoom/4).toInt+40, c= new Color(180,180,180) )
    im.drawText("1/4", (250-250*zoom/4).toInt+40,(250-250*zoom/4).toInt+40, c= new Color(180,180,180) )

    im.drawText("+1", 260,100, c= new Color(200,200,200) )
    im.drawText("-1", 260,400, c= new Color(200,200,200) )

    if (QReg.DefaultObjIsAntiClock)  im.drawText("+i", 90 ,240, c= new Color(200,200,200) )
      else im.drawText("-i", 90 ,240, c= new Color(200,200,200) )

    if (QReg.DefaultObjIsAntiClock)  im.drawText("-i", 400,240, c= new Color(200,200,200) )
      else im.drawText("+i", 400,240, c= new Color(200,200,200) )

    if (phaseNormalization)
      im.drawText("Phase norm", 380,20, c= new Color(200,200,200) )

    elt.foreach(v => { // v est l'indice de la valeur
      val (norm_, phase_) = this(v).asEuler
      val proba = zoom*(norm_ * norm_)  // probability
      var phase = normalizeAngleOrigin(phase_, offset) - math.Pi/2 // phase
      if (QReg.DefaultObjIsAntiClock) phase = -phase -math.Pi
      val x = (250 + 225*proba*math.cos(phase)).toInt
      val y = (250 + 225*proba*math.sin(phase)).toInt
      im.drawLine(250,250,x,y, new Color(255,255,255))
      val xr = 2*(math.random()*4-4).toInt
      val yr = 2*(math.random()*4-4).toInt
      im.drawFilledCircle(xr+x-15+1,yr+y-18+3, 25+(v.toString).length*8, new Color(241,192,13))
      im.drawCircle(xr+x-15+1,yr+y-18+3, 25+(v.toString).length*8, new Color(0,0,0))
      im.drawText(v.toString, xr+x-6,yr+y+8)
    })

    if (this.isInRadians) {
      im.drawText("π", 250, 485, c = new Color(250, 250, 100))
      if (!QReg.DefaultObjIsAntiClock) im.drawText("π/2", 460, 230, c = new Color(250, 250, 100))
      else im.drawText("3π/2", 460, 230, c = new Color(250, 250, 100))
      if (!QReg.DefaultObjIsAntiClock) im.drawText("3π/2", 10, 230, c = new Color(250, 250, 100))
      else im.drawText("π/2", 10, 230, c = new Color(250, 250, 100))
    } else {
      im.drawText("180", 230, 490, c = new Color(250, 250, 100))
      if (!QReg.DefaultObjIsAntiClock) im.drawText("90", 460, 230, c = new Color(250, 250, 100))
      else im.drawText("270", 460, 230, c = new Color(250, 250, 100))
      if (!QReg.DefaultObjIsAntiClock) im.drawText("270", 10, 230, c = new Color(250, 250, 100))
      else im.drawText("90", 10, 230, c = new Color(250, 250, 100))
    }
    im.drawText("0", 245,22, c= new Color(250,250,100))

    if (zoom > 1.0) im.drawText(zoom+"x", 10,485, c= new Color(250,250,100))
    if (zoom > 1.0) im.drawText(zoom+"x", 10,486, c= new Color(250,250,100))

    im.drawText(text, 6,20, c= new Color(250,250,100))

    // im.drawState(20,20,0.5, 3.1415) // test du dessin

    im.save(filename)
    this.drawAllState = svg
  } // drawImage



  def drawStateImage(filename : String="state", // name of the file + png
                     text: String="", // text to draw
                     numLines: Int = 1, // how many lines
                     osize:Int = 30, // size of one circle
                     clist : List[Int] = List() // Cond QBit list
                    ): Unit = {
    val coln = nbValues/numLines // number of col
    val bord = 60
    val im = GraphCanvas( 100+(2*osize+bord)*coln, 100+ (2*osize+bord*2)*numLines)
    im.drawText(text, 6,20, c= new Color(255,255,255))
    // List of the changed
    val lchanged = for( i <- qbitChanged.indices if (qbitChanged(i)) ) yield i
    val qbs = qbMstate.indices.filter(v => qbMstate(v) != -1).toList
    val qbs0 = qbs.filter(v => qbMstate(v) == 0).toList
    val qbs1 = qbs.filter(v => qbMstate(v) == 1).toList

      //indices.map(v => if qbitChanged(v) v else -1 ).filter(_ > -1).toList
    val phaseOrg = findPhaseOrg
    (0 until nbValues).foreach(
      v => {
        val cx = bord+(2*osize+bord)*(v % coln)
        val cy = bord+(2*osize+bord*2)*((v / coln).toInt)
        val amp = this(v).proba;
        val phase = this(v).phase(phaseOrg);
        im.drawState(50+cx,50+cy,amp,phase,osize, v, nbQbits, lchanged.toList, clist, qbs, qbs0, qbs1, QReg.DefaultObjIsAntiClock)
        im.drawText(text = v.toString, 50+cx-5,40+cy+osize+bord, c= new Color(255,255,255))
        if (this.changed(v)) {
          //im.drawFilledCircle(50+cx-5+3,48+cy+osize+bord, 5, c= new Color(250,250,250))
          im.drawLine(50+cx-5+3-10,48+cy+osize+bord,
            50+cx-5+3+14,48+cy+osize+bord, c= new Color(255,255,255))
        }
      }
    )
    im.save(filename)
  } // drawStateImage



  /** inner Op : ends the processing on a register */
  def end(): Unit = {
    if (this.myPdf != null) {
      myPdf.allCircuit(this)
      this.myPdf.closePdf()
      this.myPdf = null
    }
  }

} // QReg






object QReg {

  val LabelInfo = "VQS (G.Ménier)"
  val All = -2 // For All QBits
  val Index = -1 // Idx for labels


  val MinNorm = 0.00001  // if norm < MinNorm, use 0 instead

  val rn = new SecureRandom()
  val randomS :  SecureRandom = new SecureRandom(rn.generateSeed((40+ 30*math.random()).toInt))

  def flip(proba: Double): Boolean = {
    this.randomS.nextDouble() < proba
  } // flip

  def notifyError(msg : String): Unit = {
    println("\n<*** Error: "+msg+" ***>\n")
    System.exit(1)
  }

  // Initializing variables
  var DefaultObjIsInRadians = false;

  def setDefaultRadians() { DefaultObjIsInRadians = true }
  def setDefaultDegrees() { DefaultObjIsInRadians = false }


  // Global phase normalization for the displays, or not ?
  var DefaultObjPhaseNormalization = true;

  /** State : the drawing displays the normalized phase */
  def setDefaultDrawPhaseNormalization(): Unit = {
    DefaultObjPhaseNormalization = true
  }

  /** State : the drawing does not display the normalized phase */
  def setDefaultDrawNOPhaseNormalization(): Unit = {
    DefaultObjPhaseNormalization = false
  }


  // When states are depicted as circle, should the phase go counterWise or anticounterwise
  var DefaultObjIsAntiClock = false
  def setDefaultDrawPhaseAntiClock(): Unit = {
    DefaultObjIsAntiClock = true
  }

  def setDefaultDrawPhaseClock(): Unit = {
    DefaultObjIsAntiClock = false
  }




  // If the console doesn't manage special characters,
  // switch to full aSCII
  var DefaultObjUseASCII = false

  def setDefaultUseASCII(): Unit = {
    DefaultObjUseASCII = true
  }

  def setDefaultUseNOASCII(): Unit = {
    DefaultObjUseASCII = false
  }


  // draw only possible state (proba > 0)
  // or save some space for display
  var DefaultDrawAllState: Boolean = false

  def setDefaultDrawAll(): Unit = {
    DefaultDrawAllState = true
  }

  def setDefaultDrawOnlyPossible(): Unit = {
    DefaultDrawAllState = false
  }

} // QReg
