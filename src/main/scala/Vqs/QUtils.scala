package Vqs
// VQS : Quantum Computing Simulation
// Gildas Ménier
// 2020
// gildas.menier@univ-ubs.fr
import java.io.File
import Vqs.complex.QComplex

import scala.io.AnsiColor.{CYAN, GREEN, MAGENTA, RED, RESET, YELLOW}

/** Some utilities to convert or manage files
 */

object QUtils {

  /** helper to convert a binary string to a number */
  def binToInt(binaryString : String): Int = {
    Integer.parseInt(binaryString, 2)
  }

  /** returns a random state ._1 |0> + ._2 |1> for a QBit */
  def randomState() : (QComplex, QComplex) = {
     val dephase =  math.Pi * (1 - 2*math.random())
     val pa = math.random()
     var pb = math.sqrt(1 - pa *pa)
    ( new QComplex(pa,0),  new QComplex(pb*math.cos(dephase), pb*math.sin(dephase)))
  }


  /** helper to convert a number to a binary string */
  def toBinary(n: Int, lg_ : Int = -1): String = {
    var lg : Int = lg_
    if (lg == -1) lg = (math.ceil(math.log10(n)/math.log10(2.0)).toInt)
    @scala.annotation.tailrec
    def binary(acc: String, n: Int): String = {
      n match {
        case 0 | 1 => n+acc
        case _ => binary((n % 2)+acc, (n / 2))
      }
    } // binary
    val res = binary("",n)
    if (res.length<lg) ("0"*(lg-res.length)+res)
    else res
  } // toBinary

  /** helper to get a list of files from a dir */
  def getListOfFiles(dir: String):List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).toList
    } else {
      List[File]()
    }
  } // getListOfFiles

  /** helper to remove images from vqsimges dir */
  def rmImages = {
    getListOfFiles("vqsimages/").foreach(_.delete)
  } // removeImages

  /** helper to remove the temp image */
  def rmImageTemp = {
    getListOfFiles("vqsimages/temp").foreach(_.delete)
    val d = new File("vqsimages/temp.png")
    if (d.exists) {
      d.delete
    }
  } // removeImages


  /** helper to create a dir for images */
  def createImagesDirectoryIfNecessary = {
    val file = new File("vqsimages")
    if (! file.exists()) file.mkdir
  }

  /** helper to draw an histogr */
  def drawIntHistogram( cl : List[Int], length: Int, useASCII : Boolean = QReg.DefaultObjUseASCII) = {
    val l = cl.groupBy(identity).mapValues(_.size).toList.sortBy(_._1)
    val m:Int = (l.maxBy(_._2)._2)
    val som:Int = l.map(_._2).sum
    l.foreach(
      v => {
        val tot = 30; // nombre max de car
        val totp = (tot * v._2) / som;
        if (useASCII)
          println(
            ("\t" + v._1 + "\t|" + toBinary(v._1, length) + ">" + "\t"+  "*" * totp + "-" * (tot - totp)+ "\t"+
              v._2 + "    ("  + ((100*v._2)/som).toInt + " %)"
              ).replaceAllLiterally("*", s"${GREEN}*${RESET}"))
        else
          println(
          ("\t" + v._1 + "\t|" + toBinary(v._1, length) + ">" + "\t"+  "▓" * totp + "-" * (tot - totp)+ "\t"+
            v._2 + "    (" + ((100*v._2)/som).toInt + " %)"
            ).replaceAllLiterally("▓", s"${GREEN}█${RESET}"))
      })
  }

  /** utility to insert ansi colors in a QBit string */
  def colorizeBinary(nb: Int, r: String) : String = {
    val colorBits = Array( RED, GREEN, MAGENTA, YELLOW,  CYAN )
    var res = r
    for (i <- 0 until math.pow(2,nb).toInt) {
      val n = toBinary(i,nb)
      val nl = n.zipWithIndex.map(
        v => {
          val c = colorBits((n.length - v._2 -1) % colorBits.length)
          s"${c}${v._1}"
        }
      ).mkString+s"${RESET}"

      res = res.replaceAllLiterally("|"+n+">", "|"+nl+">")
    }
    res
  } // colorizeBinary

  /** utility to normalize an angle */
  def normalizeAngleOrigin(ang_ : Double, origin: Double = 0.0): Double = {
    var ang = (ang_ + 2*2*math.Pi - origin)+2*50*math.Pi
    while ( math.abs(ang) > math.Pi) ang = ang - 2*math.Pi;
    if ( math.abs(math.abs(ang)-math.Pi) < 0.01 ) ang = math.Pi
    // println("NormaizeAngleOrigin : " +(ang_ *180)/math.Pi + "  "+ (ang*180)/math.Pi)
    ang
  } // normalizeAngleOrigin

  /** utility to convert deg to rad */
  def cvtDegToRad(v: Double) = (v*Math.PI)/180

  /** utility to convert rad to deg */
  def cvtRadToDeg(v: Double) = (v*180)/Math.PI

  /** utility to compare phases */
  def equPhases(v1: Double, v2 : Double, deg: Boolean  = false ) = {
    var dv1=v1
    var dv2=v2
    if (!deg) {
      dv1 = (v1 / 2*Math.PI)*360
      dv2 = (v2 / 2*Math.PI)*360
    }
    // println((Math.floor(v1+ 10*360)).toInt % 360)
    // println((Math.floor(v2+ 10*360)).toInt % 360)
    dv1 = Math.floor(dv1 * 100000)*100000
    dv2 = Math.floor(dv2 * 100000)*100000

    ( ((v1+ 10*360)).toInt % 360 == ((v2+ 10*360)).toInt % 360 )
  }


  /** utility to (try to) convert a Dec to a fraction */
  def cvtDecimalToFraction(x: Double): (Int, Int) = {
    if (x < 0) {
      val r = cvtDecimalToFraction(-x)
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

  def stripStringANSI(s: String): String = {
    var res: String = ""
    res = s.replaceAll("\u001B\\[[0-9][0-9]m","")
    res = res.replaceAll("\u001B\\[[0-9]m","")
    res
  }



}
