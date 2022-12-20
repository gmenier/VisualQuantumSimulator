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

  // Array of states amp of proba (each initialized to 0+0i)
  val state: Array[QComplex] = Array.fill[QComplex](nbValues)(QComplex(0,0))

  // Array of starting QBits ( 1.|0> + 0.|1> )
  val QBitInit: Array[(QComplex, QComplex)] = Array.fill[(QComplex, QComplex)](nbValues)((1,0))

  // to keep track of the states that may have changed
  val changed: Array[Boolean] = Array.fill[Boolean](nbValues)(false)

  // to keep track of the QBits that have changed
  val qbitChanged: Array[Boolean] = Array.fill[Boolean](nbQbits)(false)

  // by default, the first QBit is 1, so the only state if 0 with a full probability
  this.state(0) = QComplex(1,0) // valeur 000 par défaut

  val NOMEASURE = -1
  // a pure state is 0 or 1 (after measure)
  val qbMState = Array.fill[Int](nbQbits)(NOMEASURE) // nostate, since no measure yet


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

  var drawBitCircle = QReg.DefaultDrawBitCircles

  var lastOp : String ="" // last operator used (trace)

  var phaseNormalization : Boolean = QReg.DefaultObjPhaseNormalization // if true normalize the phases

  var renderConsoleIDEA = ! QReg.DefaultObjUseASCII
  var onlyAscii = QReg.DefaultObjUseASCII

  var performsTraceFunction = ( idTrace : Int, thisRegister : QReg) => {  }

  def useOnlyASCII(v : Boolean): Unit = {
    this.renderConsoleIDEA = ! v
    this.onlyAscii = v
  }

  // you may use a function called for each step (0 = start, -1 = end)
  def useTraceFunction( fct : (Int, QReg) => Unit): Unit = {
    performsTraceFunction = fct
  }

  // no trace function
  def resetTraceFunction(): Unit = {
    performsTraceFunction = ( idTrace : Int, thisRegister : QReg) => {  }
  }

  // true if the states are about the same
  def sameStatesAs( r : QReg) : Boolean = {
    (this.nbQbits == r.nbQbits) &&
    (0 until nbValues).forall(
      v => this(v) == r(v)
    )
  }

  // Array of states amp of proba (each initialized to 0+0i)
  // val state: Array[QComplex] = Array.fill[QComplex](nbValues)(QComplex(0,0))

  // Array of starting QBit ( 1.|0> + 0.|1> )
  // val QBitInit: Array[QComplex] = Array.fill[(QComplex, QComplex)](nbValues)((1,1))

  //
  def computeStateGivenQbits(): Unit = {
    for( state <- 0 until nbValues)  {
      val binary = QUtils.toBinary(state, nbQbits)
      var amp = new QComplex(1)
      for( qb <- 0 until binary.length) {
          amp = amp * ( binary(binary.length -1 - qb) match {
            case '0' => QBitInit(qb)._1
            case '1' => QBitInit(qb)._2
          })
      }
      this.state(state) = amp
      // println(binary+"  "+amp)
    }
  } // computeStateGivenQbits


  // sets the state of one QBit (use only at the beginning when no QBits are intricated)
  def pokeQBitState(numQbit : Int, state: (QComplex, QComplex)): Unit = {
    if ( math.abs(1- (state._1.norm2 + state._2.norm2)) < 0.000000001 ) { // check Born's rule
      if ((numQbit >= 0) && (numQbit < nbQbits)) {
        QBitInit(numQbit) = state
        computeStateGivenQbits() // resets the state computation

        pad.writeStates(QBitInit.map(
          cp => {
            if ( cp._1.norm2 < 0.00001) "1" else if (cp._2.norm2 < 0.00001) "0" else "*"
          }
        ))

         this.lastOp = LabelOf(numQbit)+" <- |> "+state.toString()
         processTraceIfNecessary()

      } else notifyError("In pokeQBitState, check the Qbit # number")
    } else notifyError("In pokeQBitState, the state given for the Qbit state doesn't comply to the Born's rule")
  } //




  // Gets the state of one QBit : all other QBits have to be measured
  def peekQBitState(numQbit : Int, phasenorm : Boolean = true) : (QComplex, QComplex) = {
    // Check that all the QBits have been measured but this one
    if ( (getMState(numQbit) == NOMEASURE)
          && ( (0 until nbQbits).filter( _ != numQbit).forall( getMState(_) != NOMEASURE ))) {
      // The first is for |0>, then for |1>
      val collect = state.filter( _.norm2 > 0.0000000000000000001)
      if (phasenorm) {
             val fp = findPhaseOrg
             (collect(0).phaseUnShift(fp), collect(1).phaseUnShift(fp))
      } else (collect(0), collect(1))

    } else {
      notifyError("In peekQBitState, Check that only the QBit requested is left unmeasured for peek")
      (0,0)
    }

  } //




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
  def trace(sizeOfTrace : Int = 2, useASCII : Boolean = this.onlyAscii): QReg = {
    this.useOnlyASCII(useASCII)
    QUtils.createImagesDirectoryIfNecessary
    QUtils.rmImages
    isTrace = true
    traceIdx = 0
    traceSize = if ((sizeOfTrace>0) && (sizeOfTrace<this.nbValues)) sizeOfTrace else 2
    println(
      QReg.stripANSI(
         s"${BLUE}*${CYAN}*${GREEN}*${YELLOW}*${RED}*${MAGENTA}*${RESET} VQS > Starting trace "
         + (if (useASCII) " with ASCII output" else "")
      )
    )
    performsTraceFunction(0, this)
    println(this.render)
    println(QReg.stripANSI(this.toString))
    this.resetChange()
    this.drawStateImage(filename = "trace_"+traceIdx, numLines = traceSize, text="Starting trace")
   // if (this.myPdf != null) { // création d'un fichier pdf
   //   this.myPdf.writeReport(this, circuitSize, traceIdx)
   // }
    traceIdx = traceIdx+1
    this
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
  def apply(idx : String) : QComplex = this(QUtils.binToInt(idx))




  /** Gets the measured state for a reg value (pure)
   *  @param idx_ of the value
   *  @return 0, 1 or -1 (not measured)
   */
  def getMState(idx : Int) = {
    qbMState(idx)
  }


  /** Gets the measured state for the full register (should < before)
   */
  def getMQBit: Int = { // Read the reg value - should < before
    val rl : List[Int] = (for (i <- 0 until nbQbits) yield readMQBit(i)).toList
    rl.reverse.foldLeft(0)(
      (ac,v) => v + 2*ac
    )
  } // getMQbit


  /** Gets the measured state for a reg value (pure) - raises an internal error
   *  @param idx_ of the value
   *  @return 0, 1 or -1 (not measured)
   */
  def readMQBit(idx: Int) = { // reads a QBit value - should < before reading
    if (idx == QReg.All)
      getMQBit
    else {
      val r = getMState(idx)
      if (r == NOMEASURE) {
        notifyError(s"in readMQbit Trying to read the Qbit #${idx} not measured yet (try <())")
        0
      } else {
        r
      }
    }
  } // readQbit

  def ?(idx: Int = QReg.All) : Int = readMQBit(idx)




  /** Sets the measured state for a value in the register
   */
  def setMState(idx: Int, value: Int) { qbMState(idx) = value }





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
    this.update(QUtils.binToInt(idx), value)
  } // update



  // writes a value in the reg
  def write(value : Int) { // initialise the first register's value
    if ( (value > this.nbValues) || (value <0) )
      notifyError("trying to write or init to a bigger value than possible : check the number of QBits") else {
      this.state.indices.foreach(v => this (v) = QComplex(0, 0))
      this (value) = QComplex(1, 0)
      pad.writeValue(value) // valeur courante
    }
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

  // label of a Qbit (with its name if possible)
  def LabelOf(numQbit : Int): String = {
    if (numQbit == QReg.All) "All" else
    pad.getQbitLabel(numQbit)
  } //

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

        case <(idx)     => {lastOp = "<("+LabelOf(idx)+")"; forceRead(idx)}

        case F(name, fct, _, expand, skipTrace) =>
          var svgTrace = isTrace
          if (skipTrace) this.traceOff();
          if (!expand)  this.hideRender()
          fct(this);
          if (!expand) this.showRender(); this.isTrace = svgTrace
          this - VLabel("╖"+"║"*((nbQbits)*2-3)+"╜") - |(2)
          if (expand == false) {
            this - |(name.size)
          }
          lastOp = qop_p.opLabel //



        case Label(_)   =>
        case VLabel(_)  =>
        case |(_)       =>

        case $(n,s) => this.setQBitLabel(n,s)

        case |>(numQbit : Int, state: (QComplex, QComplex)) => this.pokeQBitState(numQbit, state)


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
                val s1 = QUtils.binToInt(v._2(0));
                val s2 = QUtils.binToInt(v._2(1))
                val tempC = this (s2);
                this (s2) = this (s1);
                this (s1) = tempC // swap
                // println("swap "+s1+ " and "+s2)
                qbitChanged(e1) = true
                qbitChanged(e2) = true
              }
            ) // foreach
          lastOp = qop.opLabel

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

          var idxQBits = qop.idxBit

          if ((idxQBits.size >0) && (idxQBits.max >= nbQbits)) notifyError("Accessing an unknown QBit (check the # in the operator)")

          if ((idxQBits.size>0) && (idxQBits(0) == All)) { // Multiple QBits (all QBits)
            (0 until nbQbits). foreach( v => applyOp(List(v), masque, qop) )
          } else {
            if ((idxQBits.size > 0) && (idxQBits(0) != Index )) {
              if (!idxQBits.exists( getMState(_) != NOMEASURE )) {
                applyOp(idxQBits, masque, qop)
              } else notifyError("check "+idxQBits+" some are already measured QBits")
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

      if (onlyAscii) {
        println( traceIdx + ". " + lastOp)
      } else {
        println(QReg.stripANSI( traceIdx+".  "+s"${YELLOW}"+ lastOp+s"  ${RESET} ")) //▶
      }

      performsTraceFunction(traceIdx, this) // so you can get your own trace function
      // println(if (this.renderConsoleIDEA) this.render else this.renderWithoutAnsiClean)
      println(this.render)
      println(QReg.stripANSI(this.toString))

      this.drawStateImage(filename = "trace_"+traceIdx, numLines = traceSize, text=traceIdx+ ". "+lastOp, clist= condl)
      if (this.myPdf != null) { // Creates a pdf file
          this.myPdf.writeReport(this, circuitSize, traceIdx, text = traceIdx+ ". "+lastOp)
      }
      traceIdx = traceIdx + 1
    }
  }

// performs an op
  def applyOp(idxQBits : List[Int], mask : Int, qop : QOperator ) {
    // applies the qop on the Qbits idxQBits
    // conditionnal to the idxQbit in  the mask (if mask >0)

    idxQBits.foreach( nbQb => if ( nbQb >=0 )  qbitChanged(nbQb) = true) // tags this Qbit as changed )


      lastOp =  if (mask > 0) {
        val maskList = ((0 until nbQbits).filter(i => {
          (((math.pow(2, i)).toInt) & mask) != 0
        })).map(elt => LabelOf(elt)).mkString(" "," ","")
        "C("+qop.opLabel+","+maskList+")"
      } else  qop.opLabel

      // Computes the couple of Qbits with only one V different
      val p = math.pow(2, idxQBits(0)).toInt // 2^idxQbit
      val s = (0 until nbValues).groupBy(_ / p).toList.sortBy(_._1).groupBy(_._1 % 2 == 0)
      val rp = s(true).flatMap(c => c._2.toList)
      val ri = s(false).flatMap(c => c._2.toList)
      var vr = rp.zip(ri)

      if (mask > 0) vr = vr.filter(v => (v._1 & mask) == mask)

      qop match {
        case Swap(e1,e2) => {
            val r = (0 until math.pow(2, nbQbits).toInt).
              filter( (r: Int) => ((r & mask) == mask)).
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
                  val s1 = QUtils.binToInt(v._2(0));
                  val s2 = QUtils.binToInt(v._2(1))
                  val tempC = this (s2);
                  this (s2) = this (s1);
                  this (s1) = tempC // swap
                  println("swap "+s1+ " and "+s2)
                  qbitChanged(e1) = true
                  qbitChanged(e2) = true
                }
              ) // foreach
        }

        case _ => { // Applies the op on these states with matrix
          var f: QV => QV = qop.op _

          vr.foreach {
            case (i1, i2) =>
              val v = f(QV(this (i1), this (i2))) // matrix *
              this (i1) = v(0);
              this (i2) = v(1)
          }
        }

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
  def renderRaw : String = {
    var r = pad.render()

    val colorBits = Array( RED, GREEN, MAGENTA, YELLOW,  CYAN )

    for (i <- 0 until nbQbits) {
        val nbs = i+":"
        val c = colorBits(i % colorBits.length)
        r = r.replaceAllLiterally(nbs, s"${c}${nbs}${RESET}")
    }

    if (this.onlyAscii)
      r.replaceAllLiterally("(0)", s" ${BOLD}0${RESET} ")
        .replaceAllLiterally("(1)", s" ${BOLD}1${RESET} ")
        .replaceAllLiterally("Rx","svg768")
        .replaceAllLiterally("x",s"${RED}x${RESET}")
        .replaceAllLiterally("svg768","Rx")
        .replaceAllLiterally("%0", s"${BOLD}0")
        .replaceAllLiterally("%1", s"${BOLD}1")
        .replaceAllLiterally("%*", s"${RED}*${RESET}")
        .replaceAllLiterally("│",s"${RED}|${RESET}")
        .replaceAllLiterally("╓", s"${YELLOW}|${RESET}")
        .replaceAllLiterally("║", s"${YELLOW}|${RESET}")
        .replaceAllLiterally("╙", s"${YELLOW}L${RESET}")
        .replaceAllLiterally("—", "_")
        .replaceAllLiterally("•",s"${RED}o${RESET}")
        .replaceAllLiterally("╖", s"${YELLOW}|${RESET}")
        .replaceAllLiterally("╜", s"${YELLOW}|${RESET}")  +"\n"

      else

    r.replaceAllLiterally("(0)", s" ${BOLD}0${RESET} ")
      .replaceAllLiterally("(1)", s" ${BOLD}1${RESET} ")
      .replaceAllLiterally("%0", s"${BOLD}0")
      .replaceAllLiterally("%1", s"${BOLD}1")
      .replaceAllLiterally("%*", s"${RED}*${RESET}")
      .replaceAllLiterally("╓", s"${YELLOW}╓${RESET}")
      .replaceAllLiterally("║", s"${YELLOW}║${RESET}")
      .replaceAllLiterally("╙", s"${YELLOW}╙${RESET}")
      .replaceAllLiterally("Rx","svg768")
      .replaceAllLiterally("x",s"${RED}x${RESET}")
      .replaceAllLiterally("svg768","Rx")
      .replaceAllLiterally("│",s"${RED}│${RESET}")
      .replaceAllLiterally("•",s"${RED}•${RESET}")
      .replaceAllLiterally("╖", s"${YELLOW}╖${RESET}")
      .replaceAllLiterally("╜", s"${YELLOW}╜${RESET}") + "\n"

  }

  def render() = {
    QReg.stripANSI(this.renderRaw)
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
    var rend = this.renderRaw
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
        if (line.length > 15) {
          if ((lpos > 1) && (lpos % 2 == 0) && (lpos < (nbQbits + 2) * 2))
            line.substring(0, 12) + "..." + line.substring(from, line.length - 1)
          else
            line.substring(0, 12) + "   " + line.substring(from, line.length - 1)
        } else line
      })).mkString("\n")
    }
  } // cutRender

  /** sets a label for a # QBit
   */
  def setQBitLabel(numqb : Int, name : String): Unit = {
    if (numqb < nbQbits) {
      this.pad.setQbitLabel(numqb, name)
    }
  }


  /** outputs a string with the current state of the register
    */
  override def toString : String = {

    val titrePhase =
      if (! this.onlyAscii)
               if (this.isInRadians) "Phase [-π 0 π]     " else "[-180°   0    180°]"
      else     if (this.isInRadians) "Phase [-Pi 0 Pi] "   else "[-180    0    180 ]"

    val startAng = if (phaseNormalization) findPhaseOrg  else 0.0 // Normalizes

    var elt : List[Int] = (0 until nbValues).toList

    if (!this.drawAllState) elt = elt.filter( n => Math.abs(this(n).asEuler._1) > 1E-10 )

    var title = if (! this.onlyAscii)
      "Probability   "+" "*6+"\t\t"+titrePhase+" "*5+ "V\t\tBasis\t\t\ta+ib\t\t\t\t\t\t\t\t(r.ei Θ)"
    else
      "Probability   "+" "*6+"\t    "+titrePhase+" "*5+ "\tV\tBasis\t\ta+ib\t\t\t\t\t(r.ei O)"

    var res = title +"\n"+
      elt.map(v => this(v).probaString(isEmpty = (this(v).norm < QReg.MinNorm), ascii = this.onlyAscii)+" "+
        { if (this(v).norm < QReg.MinNorm) this(v).phaseString(startAng,0, ascii = this.onlyAscii)
        else                               this(v).phaseString(startAng, ascii = this.onlyAscii)
        } +
        "\t\t"+
        (v.toString+"         ").substring(0,5)+
                              "\t|"+(QUtils.toBinary(v,nbQbits)+">\t"+" "*10).substring(0,12) +
        {
          if (this(v).norm < QReg.MinNorm)  "  ."+" "*29 else (this(v).toString+" "*30).substring(0,30)
        } + {
          if (this(v).norm < QReg.MinNorm) "\t." else "\t\t"+this(v).asEulerString(this.isInRadians,startAng, this.onlyAscii)
        } + {if (phaseNormalization) {
             if (startAng != 0.0 )
               s"\t ${MAGENTA}-> " + this (v).asPhaseNormString(this.isInRadians, startAng, this.onlyAscii) +s" ${RESET}"
               else ""
              }
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
      if (this.isInRadians) a else cvtDegToRad(a)
  }


  // creates an image and saves it
  // TODO autozoom
  def drawCircleImage(filename : String ="registre", zoomparam : Double = 0, text: String = ""): Unit = {
    val im = GraphCanvas()
    var svg = this.drawAllState

    drawOnlyPossible()
    var offset:Double = 0.0
    if (phaseNormalization)
      offset = findPhaseOrg // normalizes

    var elt : List[Int] = (0 until nbValues).toList

    // auto zoom if zoom == 0
    val zoom = if (zoomparam == 0) {
      val mx = elt.map(v => {
        val (norm_, _) = this (v).asEuler; norm_
      }).max
      if (mx > 0)   1/(mx*mx)
      else 1.0
    } else zoomparam

    if (!this.drawAllState) elt = elt.filter( n => Math.abs(this(n).asEuler._1) > 1E-10 )

    im.drawFilledCircle(0,0,500, new Color(0,0,200))
    im.drawFilledCircle(25,25,500-50, new Color(0,200,250))

    for( i <- ((25 until 250) by 10).reverse)
      im.drawFilledCircle(250-i,250-i,i*2, new Color(0,0,i))

    im.drawFilledCircle(250-4,250-4,4*2, new Color(241,192,13))

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

    elt.foreach(v => { // v est l'indice de l'état
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

    if (zoom > 1.0) im.drawText(f"$zoom%3.2f"+"x", 10,485, c= new Color(250,250,100))
    if (zoom > 1.0) im.drawText(f"$zoom%3.2f"+"x", 10,486, c= new Color(250,250,100))

    im.drawText(text, 6,20, c= new Color(250,250,100))

    // im.drawState(20,20,0.5, 3.1415) // test du dessin

    im.save(filename)
    this.drawAllState = svg
  } // drawImage



  def drawStateImage(filename : String="state", // name of the file + png
                     text: String="", // text to draw
                     numLines: Int = 1, // how many lines
                     osize:Int = 30, // size of one circle
                     clist : List[Int] = List(),
                     coloringScheme : Int = ColoringSchemeChanged// Cond QBit list
                    ): Unit = {
    val coln = nbValues/numLines // number of col
    val bord = 60
    val im = GraphCanvas( 100+(2*osize+bord)*coln, 100+ (2*osize+bord*2)*numLines)
    im.drawText(text, 6,20, c= new Color(255,255,255))
    // List of the changed
    val lchanged = for( i <- qbitChanged.indices if (qbitChanged(i)) ) yield i
    // qbMState(i) is the value measured for the # state i (res is 0 or 1 or -1)
    val qbs = qbMState.indices.filter(v => qbMState(v) != -1).toList
    // qbs is the set of QBits read
    val qbs0 = qbs.filter(v => qbMState(v) == 0).toList
    // qbs0 is the set of Qbit to 0
    val qbs1 = qbs.filter(v => qbMState(v) == 1).toList
    // qbs1 is the set of Qbit measured to 1

      //indices.map(v => if qbitChanged(v) v else -1 ).filter(_ > -1).toList

    val phaseOrg = if (phaseNormalization)  findPhaseOrg else 0.0

    // print("QB0 : "+ qbs0.mkString(" ")+"\n")
    // print("QB1 : "+ qbs1.mkString(" ")+"\n")

    (0 until nbValues).foreach( // for each state
      v => {
        val cx = bord+(2*osize+bord)*(v % coln)
        val cy = bord+(2*osize+bord*2)*((v / coln).toInt)
        val amp = this(v).proba;
        val phase = this(v).phase(phaseOrg);
        val nbCircles = if (drawBitCircle) nbQbits else 1

        if (coloringScheme == ColoringSchemeChanged) {
          if (this.changed(v)) {
            im.drawState(50 + cx, 50 + cy, amp, phase, osize, v, nbCircles, lchanged.toList, clist, qbs, qbs0, qbs1, QReg.DefaultObjIsAntiClock, QReg.DrawRed)
          } else {
            im.drawState(50 + cx, 50 + cy, amp, phase, osize, v, nbCircles, lchanged.toList, clist, qbs, qbs0, qbs1, QReg.DefaultObjIsAntiClock, QReg.DrawGreen)
          }
        }

        //im.drawState(50+cx,50+cy,amp,phase,osize, v, nbCircles, lchanged.toList, clist, qbs, qbs0, qbs1, QReg.DefaultObjIsAntiClock, QReg.DrawGreen)
        im.drawText(text = v.toString, 50+cx-5,40+cy+osize+bord, c= new Color(255,255,255))
        if ((drawBitCircle && this.changed(v)) || ( (this.changed(v)) ) ) {
          //im.drawFilledCircle(50+cx-5+3,48+cy+osize+bord, 5, c= new Color(250,250,250))
         // im.drawLine(50+cx-5+3-10,48+cy+osize+bord,
         //   50+cx-5+3-10,48+cy+osize+bord-14, c= new Color(255,255,255))
          im.drawLine(50+cx-5+3-10,48+cy+osize+bord,
            50+cx-5+3+14,48+cy+osize+bord, c= new Color(255,255,255))
        }
      }
    )
    im.save(filename)
  } // drawStateImage



  /** inner Op : ends the processing on a register */
  def end(): Unit = {
    if (this.isTrace) {
      performsTraceFunction(-1, this)
    }
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

  val randomS = new java.util.SplittableRandom()

  // Coloring Schemes for circledrawing (in GraphCanvas)
  val DrawGreen = 0
  val DrawRed = 1
  val DrawJump = 3

  val ColoringSchemeChanged = 0
  val ColoringSchemeAllGreen = 1


  def flip(proba: Double): Boolean = {
    this.randomS.nextDouble() < proba
  } // flip

  def notifyError(msg : String): Unit = {
    println(QReg.stripANSI("\n<*** Error: "+msg+" ***>\n"))
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
  var DefaultObjIsAntiClock = true
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


  // Switch to no ANSI COLOR
  var DefaultObjUseColor = false

  def setDefaultUseColor(): Unit = {
    DefaultObjUseColor = true
  }

  def setDefaultUseNOColor(): Unit = {
    DefaultObjUseColor = false
  }

  // remove ANSI Coding if necessary
  def stripANSI(s: String): String = {
    if (DefaultObjUseColor) s else QUtils.stripStringANSI(s)
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

  var DefaultDrawBitCircles : Boolean = false
  def setDefaultDrawBitCircles(): Unit = {
    DefaultDrawBitCircles = true
  }
  def setDefaultDontDrawBitCircles(): Unit = {
    DefaultDrawBitCircles = false
  }




} // QReg
