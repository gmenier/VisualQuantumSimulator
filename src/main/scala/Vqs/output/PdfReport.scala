package Vqs.output

// VQS : Quantum Computing Simulation
// Gildas Ménier
// 2020
// gildas.menier@univ-ubs.fr


import org.apache.pdfbox.pdmodel.common.PDRectangle
import org.apache.pdfbox.pdmodel.font.PDType1Font
import org.apache.pdfbox.pdmodel.graphics.image.PDImageXObject
import org.apache.pdfbox.pdmodel.{PDDocument, PDPage, PDPageContentStream}

import Vqs._

class PdfReport(fileName : String = "file.pdf", docLabel : String="") {

  var document : PDDocument = new PDDocument
  var contentStream : PDPageContentStream = null

  var vpos = 550 // base en 50 pts
  var xpos = 25
  var leading = 10
  var fontSize = 10
  var numPage = 1

  val calcW = 550 // max size for the pictures

  def getPos = vpos

  def newPage() {

    vpos = 550
    val page = new PDPage(new PDRectangle(PDRectangle.A4.getHeight, PDRectangle.A4.getWidth))
    // val page = new PDPage
    this.document.addPage(page)

    this.contentStream = new PDPageContentStream(document, page)
    this.contentStream.setFont(PDType1Font.COURIER_BOLD, fontSize)

    beginText()
      this.contentStream.newLineAtOffset(650, -530)
      this.contentStream.showText(QReg.LabelInfo + " (" + numPage + ")")
      this.contentStream.newLineAtOffset(xpos, vpos)
    endText()
    beginText()
      this.contentStream.newLineAtOffset(0, -530)
      this.contentStream.showText(docLabel)
      this.contentStream.newLineAtOffset(xpos, vpos)
    endText()
    numPage = numPage+1

  } // newPage


  def beginText(): Unit = {
    this.contentStream.beginText()
    this.contentStream.newLineAtOffset(xpos, vpos)
    this.contentStream.setLeading(leading)
  }

  def writeLine( s: String): Unit = {
    this.contentStream.showText( /* vpos + " ="+ */ s)
    this.cr()
  }

  def cr(): Unit = {
    this.contentStream.newLine()
    vpos = (vpos - leading).toInt
  }

  def endText(): Unit = {
    this.contentStream.endText()
  }

  var nbrp = 0

  def writeReport(rr : QReg, lgcar: Int, idxImage: Int, text: String =""): Unit = {
    if ((getPos < 100) || (nbrp>=2)) {
      this.contentStream.close()
      nbrp = 0;
      newPage();
    }
    beginText()
    if (text != "") { writeLine(text); cr(); }
      rr.cutRenderWithoutAnsi(lgcar).split("\n").foreach(
      l => writeLine(l)
    )
    for( i <- 0 until 15) cr()
    endText()

    // fix the image size TODO
    val calcH = 5*3*14 + 40 //
    // insertImage("trace_"+idxImage+".png", lgcar*7, getPos, calcW+40, calcH)

  }

  def allCircuit(rr : QReg): Unit = {
    if (this.contentStream != null) this.contentStream.close()
    newPage();

    val lg = 120
    val e1 = rr.cutRenderWithoutAnsi(200000).split("\n").flatMap(
      l => {
        val pr = (l.length/lg).toInt
        for( i <- 0 to pr) yield (
          if ((i*lg+lg) <= l.length) l.substring(i*lg,i*lg+lg) else l.substring(i*lg, l.length)
          )
      }.filter(_.length>0).zipWithIndex
    ).groupBy(_._2).toList.sortBy(_._1).map( e => e._2 )

    beginText()
    e1.foreach( pack => {
      var n = 0
      pack.foreach(l => {

       if ((l._2 > 0) && (n > -1) && (n % 2 == 0)) {
         val cs = ("    "+n/2)
         val c = cs.substring(cs.length-4,cs.length)+":... "
         writeLine(c+l._1)
         n = n+1
         if (n > (rr.nbQbits-1)*2) n = -1
       } else {
         if (n != -1) n= n+1
         writeLine("         "+l._1)
       }
      })
      if (getPos < 200) { endText; this.contentStream.close(); newPage(); beginText() } else { cr(); cr() }
    }
    )
    endText()
  }

  def closePdf() {
    this.contentStream.close()

    this.document.save(this.fileName)
    this.document.close()
    QUtils.rmImageTemp
  } // closePdf

  case class rvb(r: Int, v: Int, b: Int);

  val hotRed       = rvb(255,100,80)
  val paleRed      = rvb(150, 0,0)
  val hotGreen     = rvb(50,255,50)
  val paleGreen    = rvb(0, 120, 0)
  val black        = rvb(0,0,0)
  val magenta      = rvb(150,0,150)
  val palemagenta  = rvb(50,5,50)
  val paleblue     = rvb(0,0,180)
  val backBlue     = rvb(0,0,150)


  def insertImage(fn : String, x: Int, y:Int, w_ : Int, h_ : Int) {

    import java.awt.Color

    import javax.imageio.ImageIO

    val inputFile = ImageIO.read(new java.io.File("vqsimages/"+fn))

    for (x <- 0 until inputFile.getWidth) {
      for (y <- 0 until inputFile.getHeight) {
        val rgba = inputFile.getRGB(x, y)
        var col = new Color(rgba, true)
        val ccol = rvb(col.getRed, col.getGreen, col.getBlue)
        val rvb(r,v,b) = ccol match {
          case rvb(255, 255, 240) => rvb(0,0,0)
          case rvb(0, 0, 10)      => rvb(0,0,0)
          case rvb(0,0,0)         => rvb(255,255,255)
          case rvb(150, 0,0) /* paleRed */  => rvb(210,210,210)
          case rvb(255,100,80) /* hotRed */ => rvb(210,210,210)
          case rvb(255,255,255)   => rvb(0,0,0)
          case rvb(0,0,150)       => rvb(255,255,255)
          case rvb(0,0,130)       => rvb(230,230,230) // deep blue -> gray (fills the circle)
          case rvb(r,v,b)         => rvb(r, v,  b)
        }
        col = new Color(r,v,b)
        inputFile.setRGB(x, y, col.getRGB)
      }
    }

    val outputFile = "vqsimages/temp.png"
    ImageIO.write(inputFile, "png", new java.io.File(outputFile) )

      val pdImage = PDImageXObject.createFromFile(outputFile, document)
      val H = pdImage.getHeight()
      val W = pdImage.getWidth()
      val ri : Double = (W  / H.toDouble )
      val re : Double = (w_ / h_.toDouble)
      var h = h_
      var w = (h_ * ri).toInt
      if (w > w_) {
        w = w_
        h = (w_ / ri ).toInt
      }
      this.contentStream.drawImage(pdImage, x,y + (h_ -h), w, h)

  } // insertImage


} // pdfReport
