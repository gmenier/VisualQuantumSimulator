package Vqs
// VQS : Quantum Computing Simulation
// Gildas Ménier
// 2020
// gildas.menier@univ-ubs.fr
import java.io.File

import scala.io.AnsiColor.{CYAN, GREEN, MAGENTA, RED, RESET, YELLOW}

/** Some utilities to convert or manage files
 */

object QUtils {

  /** helper to convert a binary string to a number */
  def binaryToInt(binaryString : String): Int = {
    Integer.parseInt(binaryString, 2)
  }

  /** helper to convert a number to a binary string */
  def toBinary(n: Int, lg : Int): String = {
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
  def removeImages = {
    getListOfFiles("vqsimages/").foreach(_.delete)
  } // removeImages

  /** helper to remove the temp image */
  def removeImageTemp = {
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
  def drawIntHistogram( cl : List[Int], lg: Int) = {
    val l = cl.groupBy(identity).mapValues(_.size).toList.sortBy(_._1)
    val m:Int = (l.maxBy(_._2)._2)
    l.foreach(
      v => {
        val tot = 20; // nombre max de car
        val totp = (tot * v._2) / m;
        println(
          ("\t" + v._1 + "\t|" + toBinary(v._1, lg) + ">" + "\t"+  "▓" * totp + "▒" * (tot - totp)+ "\t"+ v._2 + " x"
            ).replaceAllLiterally("▓", s"${GREEN}▓${RESET}"))
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
  def convertDegToRad(v: Double) = (v*Math.PI)/180

  /** utility to convert rad to deg */
  def convertRadToDeg(v: Double) = (v*180)/Math.PI

  /** utility to (try to) convert a Dec to a fraction */
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



}
