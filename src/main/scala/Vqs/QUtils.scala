package Vqs

import java.io.File

import scala.io.AnsiColor.{CYAN, GREEN, MAGENTA, RED, RESET, YELLOW}

object QUtils {

  def binaryToInt(binaryString : String): Int = {
    Integer.parseInt(binaryString, 2)
  }

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

  def getListOfFiles(dir: String):List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).toList
    } else {
      List[File]()
    }
  } // getListOfFiles


  def removeImages = {
    getListOfFiles("vqsimages/").foreach(_.delete)
  } // removeImages

  def createImagesDirectoryIfNecessary = {
    val file = new File("vqsimages")
    if (! file.exists()) file.mkdir
  }

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


}
