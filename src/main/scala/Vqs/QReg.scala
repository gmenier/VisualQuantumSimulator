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


import QComplex._
import QReg._

import io.AnsiColor._



case class QReg(val nbQbits : Int) { //

  val NOPURESTATE = -1

  // 2^nbQbits complex = 0
  val nbValues : Int = scala.math.pow(2, nbQbits).toInt
  val state: Array[QComplex] = Array.fill[QComplex](nbValues)(QComplex(0,0))
  val changed: Array[Boolean] = Array.fill[Boolean](nbValues)(false)
  val qbitChanged: Array[Boolean] = Array.fill[Boolean](nbQbits)(false)
  this.state(0) = QComplex(1,0) // valeur 000 par défaut

  val qbstate = Array.fill[Int](nbQbits)(NOPURESTATE) // nostate


  def isInRadians = QReg.isRadian

  var isTrace = false
  var traceIdx = 0
  var traceSize = 0

  val circuitSize = 30

  var isShowRender : Boolean = true

  var drawAllState : Boolean = false; // if false only draw states with probability > 0

  var phaseNormalization : Boolean = true // if true normalize the phases

  def resetChange(): Unit = {
    changed.indices.foreach( v => changed(v) = false)
    qbitChanged.indices.foreach( v => qbitChanged(v) = false)
  }

  def traceOn() { this.isTrace = true }
  def traceOff() { this.isTrace = false }

  def hideRender() { this.isShowRender = false }
  def showRender() { this.isShowRender = true }

  var myPdf: PdfReport = null


  def pdf(fn : String, docLabel: String=""): Unit = {
    this.myPdf = new PdfReport(fn, docLabel)
    this.myPdf.newPage()
  }


  def trace(traceSize_ : Int = 2): Unit = {
    QUtils.createImagesDirectoryIfNecessary
    QUtils.removeImages
    isTrace = true
    traceIdx = 0
    traceSize = traceSize_
    println(">QSim : Tracing init")
    println(this.render)
    println(this)
    this.resetChange()
    this.drawStateImage(filename = "trace_"+traceIdx, numLines = traceSize, text="Trace : init")
    if (this.myPdf != null) { // création d'un fichier pdf
      this.myPdf.writeReport(this, circuitSize, traceIdx)
    }
    traceIdx = traceIdx+1
  }

  def drawPhaseNormalization(): Unit = {
    phaseNormalization = true
  }


  def drawNOPhaseNormalization(): Unit = {
    phaseNormalization = false
  }

  val pad = new QPad(nbQbits, this)

  // get the states
  def apply(idx : Int): QComplex = this.state(idx)
  def apply(idx : String) : QComplex = this.state(QUtils.binaryToInt(idx))

  def getState(idx : Int) = {
    qbstate(idx)
  }

  def readQbit(idx: Int) = { // reads a QBit value - should < before reading
    val r = getState(idx)
    if (r == -1) {
      notifyError(s"Trying to read the Qbit #${idx} not fixed yet (try D())")
      0
    } else {
      r
    }
  } // readQbit

  def readQbit: Int = { // Read the reg value - should < before
    val rl : List[Int] = (for (i <- 0 until nbQbits) yield readQbit(i)).toList
    rl.reverse.foldLeft(0)(
      (ac,v) => v + 2*ac
    )
  } // readQbit

  def setState(idx: Int, value: Int) { qbstate(idx) = value }

  def drawOnlyPossible(): Unit = { // only draws proba >0
    this.drawAllState = false
  } // drawOnlyPossible

  def drawAll(): Unit = { /// draws ALL states
    this.drawAllState = true;
  } // drawAll

  // change the states
  def update(idx : Int, value: QComplex) {
    this.state(idx) = value
    this.changed(idx) = true
  } // update

  def update(idx : String, value: QComplex) {
    this.update(QUtils.binaryToInt(idx), value)
  } // update

  def write(value : Int) { // initialise the first register's value
    // TODO qbitchanged
    this.state.indices.foreach( v => this(v)= QComplex(0,0))
    this(value) = QComplex(1,0)
    pad.writeValue(value) // valeur courante
  } // write

  def init(value: Int): Unit = {
    this.write(value)
    processTraceIfNecessary()
  }

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
        res('1').foreach(v => this (v._1) = QComplex(0, 0)) // on annule les probas de 1
      } else { // a 1
        res('0').foreach(v => this (v._1) = QComplex(0, 0)) // on annule les probas de 0
      }

      normalize()
      val vf = if (f) 0 else 1
      setState(idx, vf)
    }
  } // read

  def infoline(s: String): Unit = {
    this.pad.infoline("  "+s)
  } //infoline

  def -(qop_ : QOperator): QReg =  {

    this.resetChange() // keeps a trace of the states

    var condl = List[Int]() // Conditionnal QBit list


      qop_.setRegister(this)
      qop_.init()
      var qop: QOperator = qop_
      pad.add(qop, isShowRender)

      qop match {

        case <(idx)     => forceRead(idx)

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
            if (getState(idxQBit) != -1) {
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

  def processTraceIfNecessary(condl : List[Int] = List()): Unit = {
    if (isTrace) {
      println("_"*60+"\n")
      println("Trace : "+traceIdx)
      println(this.render)
      println(this)
      this.drawStateImage(filename = "trace_"+traceIdx, numLines = traceSize, text="Trace : "+traceIdx, clist= condl)
      if (this.myPdf != null) { // Creates a pdf file
          this.myPdf.writeReport(this, circuitSize, traceIdx)
      }
      traceIdx = traceIdx + 1
    }
  }

  def applyOp(idxQBit : Int, masque : Int, qop : QOperator ) {
      qbitChanged(idxQBit) = true

      // Computes the couple of Qbits with only one V different
      val p = math.pow(2, idxQBit).toInt
      val s = (0 until nbValues).groupBy(_ / p).toList.sortBy(_._1).groupBy(_._1 % 2 == 0)
      val rp = s(true).flatMap(c => c._2.toList)
      val ri = s(false).flatMap(c => c._2.toList)
      var vr = rp.zip(ri)

      if (masque >= 0) vr = vr.filter(v => (v._1 & masque) > 0)

      // Applies the op on these states
      var f: QV => QV = qop.op _

      vr.foreach {
        case (i1, i2) =>
          val v = f(QV(this (i1), this (i2)))
          this (i1) = v(0);
          this (i2) = v(1)
      }
    }

  def normalize() { // Born rule
    val s = math.sqrt(this.state.foldLeft(0.0)( (a,c) => a+c.norm2))
    this.state.indices.foreach(
      i => this.state(i) = this.state(i)/s
    )
  } // normalize

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



  def cutRenderWithoutAnsi(dSize : Int) : String = {
    var rend = this.render
      .replaceAll("\\x1B...?m", "")
      .replaceAll("│", "|")
      .replaceAll("║", "|").replaceAll("╓", "|")
      .replaceAll("╜", "|").replaceAll("╖", "|").replaceAll("╙","-")
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

  override def toString : String = {

    var startAng:Double = 0.0

    var svgRadian = QComplex.isRadian
    QComplex.isRadian = QReg.isRadian

    val titrePhase = if (QComplex.isRadian) "Phase [-π 0 π]     " else "[-180°   0    180°]"

    if (phaseNormalization)
        startAng = findPhaseOrg  // Normalizes

    var elt : List[Int] = (0 until nbValues).toList
    
    if (!this.drawAllState) elt = elt.filter( n => Math.abs(this(n).asEuler._1) > 1E-10 )
    var res ="Proba [0 -> 1]"+" "*6+titrePhase+" "*5+ "V\t    Bin\t\t\t    α\t\t\t\t\t\t\t\t|r|ei Θ" +"\n"+
      elt.map(v => this(v).probaString+" "+this(v).phaseString(startAng)+"\t\t"+
        (v.toString+"     ").substring(0,5)+
                              "\t|"+(QUtils.toBinary(v,nbQbits)+">\t"+" "*10).substring(0,12) +
        (this(v).toString+" "*30).substring(0,30) +
                              "\t= "+this(v).asEulerString(startAng)
    ).mkString("\n")
    QComplex.isRadian = svgRadian
    res = QUtils.colorizeBinary(nbQbits, res)
    res+"\n"
  } // toString

  def findPhaseOrg: Double = { // finds the phase reference
    var it = 0;
    while ((it < nbValues) && ( math.abs(this(it).proba)) < 0.000000001) it = it+1
    if ( it < nbValues) this(it).phase() else this(0).phase()
  }

  def angle(a :Double) = { // converts angle - all the computations are in Radians
      if (QReg.isRadian) a else convertDecToRad(a)
  }

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

    im.drawText("+1", 250,100, c= new Color(200,200,200) )
    im.drawText("-1", 250,400, c= new Color(200,200,200) )
    im.drawText("-i", 90 ,250, c= new Color(200,200,200) )
    im.drawText("+i", 400,250, c= new Color(200,200,200) )

    if (phaseNormalization)
      im.drawText("Phase norm", 380,20, c= new Color(200,200,200) )

    elt.foreach(v => { // v est l'indice de la valeur
      val (norm_, phase_) = this(v).asEuler
      val proba = zoom*(norm_ * norm_)  // probability
      val phase = normalizeAngleOrigin(phase_, offset) - math.Pi/2 // phase
      val x = (250 + 225*proba*math.cos(phase)).toInt
      val y = (250 + 225*proba*math.sin(phase)).toInt
      im.drawLine(250,250,x,y, new Color(255,255,255))
      val xr = 2*(math.random()*4-4).toInt
      val yr = 2*(math.random()*4-4).toInt
      im.drawFilledCircle(xr+x-15+1,yr+y-18+3, 25+(v.toString).length*8, new Color(241,192,13))
      im.drawCircle(xr+x-15+1,yr+y-18+3, 25+(v.toString).length*8, new Color(0,0,0))
      im.drawText(v.toString, xr+x-6,yr+y+8)
    })

    im.drawText("π", 250,485, c= new Color(250,250,100))
    im.drawText("0", 250,22, c= new Color(250,250,100))

    if (zoom > 1.0) im.drawText("Zoom "+zoom+"x", 10,485, c= new Color(250,250,100))
    if (zoom > 1.0) im.drawText("Zoom "+zoom+"x", 10,486, c= new Color(250,250,100))

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
    val qbs = qbstate.indices.filter( v => qbstate(v) != -1).toList
    val qbs0 = qbs.filter(v => qbstate(v) == 0).toList
    val qbs1 = qbs.filter(v => qbstate(v) == 1).toList

      //indices.map(v => if qbitChanged(v) v else -1 ).filter(_ > -1).toList
    val phaseOrg = findPhaseOrg
    (0 until nbValues).foreach(
      v => {
        val cx = bord+(2*osize+bord)*(v % coln)
        val cy = bord+(2*osize+bord*2)*((v / coln).toInt)
        val amp = this(v).proba;
        val phase = this(v).phase(phaseOrg);
        im.drawState(50+cx,50+cy,amp,phase,osize, v, nbQbits, lchanged.toList, clist, qbs, qbs0, qbs1)
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

  def end(): Unit = {
    if (this.myPdf != null) {
      myPdf.allCircuit(this)
      this.myPdf.closePdf()
    }
  }

} // QReg



object QReg {

  val LabelInfo = "VQS (G.Ménier)"
  val All = -2 // indicateur idx pour sélectionner tous les Qbits
  val Index = -1 // idx pour label

  var isRadian = false

  val rn = new SecureRandom()
  val randomS :  SecureRandom = new SecureRandom(rn.generateSeed((40+ 30*math.random()).toInt))

  def setRadians() { isRadian = true }
  def setDegrees() { isRadian = false }


  def flip(proba: Double): Boolean = {
    this.randomS.nextDouble() < proba
  } // flip

  def notifyError(msg : String): Unit = {
    println("\n<*** Error: "+msg+" ***>\n")
    System.exit(1)
  }

} // QReg
