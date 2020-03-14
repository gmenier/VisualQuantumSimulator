package Vqs
// VQS : Quantum Computing Simulation
// Gildas Ménier
// 2020
// gildas.menier@univ-ubs.fr
import java.awt.geom.{Arc2D, Ellipse2D, Line2D}
import java.awt.image.BufferedImage
import java.awt.{BasicStroke, Color, Font}

case class GraphCanvas(w: Int=500, h: Int=500) {

  var size : (Int, Int) = (w, h)
  val canvas = new BufferedImage(size._1, size._2, BufferedImage.TYPE_INT_RGB)

  var g = canvas.createGraphics()

  val hotRed    = new Color(255,100,80)
  val paleRed   = new Color(150, 0,0)
  val hotGreen  = new Color(50,255,50)
  val paleGreen = new Color(0, 120, 0)
  val black     = new Color(0,0,0)
  val magenta   = new Color(150,0,150)
  val palemagenta= new Color(50,5,50)
  val paleblue= new Color(0,0,180)

  // clear background
  g.setColor(Color.BLACK)
  g.fillRect(0, 0, canvas.getWidth, canvas.getHeight)

  // g.setRenderingHint(java.awt.RenderingHints.KEY_ANTIALIASING, java.awt.RenderingHints.VALUE_ANTIALIAS_ON)

  def save(fn : String): Unit = {
    g.dispose()
    javax.imageio.ImageIO.write(canvas, "png", new java.io.File("vqsimages/"+fn+".png"))
  } // save


  def drawFilledCircle(x: Int, y: Int, r: Int,   c: Color = new Color(0,0,0)): Unit = {
    // draw two filled circles
    g.setColor(c)
    g.fill(new Ellipse2D.Double(x, y, r, r))
  } // drawFillesCircle


  def drawCircle(x: Int, y: Int, r: Int,   c: Color = new Color(0,0,0)): Unit = {
    // draw two filled circles
    g.setColor(c)
    g.draw(new Ellipse2D.Double(x, y, r, r))
  } // drawFillesCircle



  def drawFilledRectangle(x: Int, y: Int, w: Int, h: Int,   c: Color = new Color(0,0,0)): Unit = {
    g.setColor(c)
    g.fillRect(x, y, w,h)
  } // drawFillesCircle


  def drawArc(x: Int, y: Int, r: Int, start: Int, extend: Int, c: Color = new Color(0,0,0)): Unit = {
    g.setColor(c)
    g.draw(new Arc2D.Double(x,y,r,r,start,extend,0))
  } // drawFillesCircle

  def drawLine(x: Int, y: Int, z: Int, t: Int, c: Color= new Color(0,0,0)): Unit = {
    g.setStroke(new BasicStroke()) // reset to default
    g.setColor(c) // same as Color.BLUE
    g.draw(new Line2D.Double(x,y, z, t))
  } // drawLine

  def drawText(text: String, x: Int, y: Int, f: Font = new Font("Calibri", Font.PLAIN, 20), c : Color= new Color(0,0,0)): Unit = {
    g.setColor(c)
    g.setFont(f)
    g.drawString(text, x, y)
  } // drawText



  def drawState(x : Int, y : Int,
                amplitude: Double, phase : Double,
                osize: Int =20,
                v_ : Int,
                nb: Int,
                lchanged : List[Int] = List(),
                lcond: List[Int] = List(),
                qbState : List[Int] = List(),
                qb0 : List[Int] = List(),
                qb1 : List[Int] = List()
               ): Unit = { // draws a state as a picture

    var ante = false;
    var red = paleRed;
    var green = paleGreen;
    val s = 4
    var exclude = false

    (0 until nb).reverse.foreach(
      v => {
        ante = true // false for real
        if (lchanged.contains(v)) {
          red = hotRed; green = hotGreen;
        } else {
          red = paleRed; green = paleGreen
        }

        val v1 = v+1
        if ((v_ & (math.pow(2,v).toInt)) == 0) {
          if (ante || (v==0))
              drawFilledCircle(x-(osize)- v1*s,y-(osize)-v1*s,osize*s/2+v1*s*2, red)
          if (qb1.contains(v)) exclude = true
        } else {
          ante = true
          drawFilledCircle(x-(osize) -v1*s,y-(osize)-v1*s,osize*s/2+v1*s*2, green)
          if (qb0.contains(v)) exclude = true
        }
        drawCircle(x-(osize)- v1*s,y-(osize)-v1*s,osize*s/2+v1*s*2, new Color(0,0,0))

      }
    )

    if (amplitude > 0.000001)
      drawFilledCircle(x-(osize),y-(osize),osize*2, new Color(0,0,150))
    else
      drawFilledCircle(x-(osize),y-(osize),osize*2, Color.BLACK)

    // sqrt to boost the size
    val camp = (math.sqrt(amplitude)*osize).toInt
    drawFilledCircle(x-(camp),y-(camp),camp*2, new Color(0,160,250))

    val campk= (amplitude*osize).toInt
    drawFilledCircle(x-(campk),y-(campk),campk*2, new Color(0,200,250))


    val cx = (x+osize*math.cos(phase- math.Pi/2)).toInt
    val cy = (y+osize*math.sin(phase- math.Pi/2)).toInt

    val cx2 = (x+amplitude*osize*math.cos(phase- math.Pi/2)).toInt
    val cy2 = (y+amplitude*osize*math.sin(phase- math.Pi/2)).toInt

    if (amplitude > 1E-10) {
      import java.awt.{BasicStroke, Color, Graphics2D}
      import java.awt.geom.Line2D

      val g2 = g.asInstanceOf[Graphics2D]
      g2.setPaint(new Color(255, 255, 240))
      g2.setStroke(new BasicStroke(5))
      g2.draw(new Line2D.Float(x,y,cx,cy))
      g2.setStroke(new BasicStroke(1))

      //drawLine(x, y, cx2, cy2, new Color(0, 0, 0))
      //drawLine(cx2, cy2, cx, cy, new Color(0, 0, 0))
    }
    // drawArc(x-(osize),y-(osize),osize*2, 180-45, 180+45, new Color(200,200,150))
    (0 until nb). foreach (
      v => {
        if (lcond contains v) {
          drawFilledCircle(x - 3, y - (osize) - 3- 6*v, 6, Color.BLACK)
          if ((v_ & (math.pow(2,v).toInt)) == 0) {
            drawCircle(x - 3, y - (osize) - 3  - 6*v, 6, Color.WHITE)
          } else {
            drawFilledCircle(x - 3, y - (osize) - 3 - 6*v, 6, Color.WHITE)
            drawCircle(x - 3 -1, y - (osize) - 3 -1  - 6*v, 6 +1, Color.BLACK)
          }
        }
      })

    if (exclude == true)
      drawFilledRectangle(x-(osize)- 7*s,y-(osize)-7*s,osize*s/2+7*s*2,(osize*s/2+7*s*2)/2 , Color.BLACK)

  } // drawState


}
