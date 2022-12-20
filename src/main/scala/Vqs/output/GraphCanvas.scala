package Vqs.output

import java.awt.geom.{Arc2D, Ellipse2D, Line2D}
import java.awt.image.BufferedImage
import java.awt.{BasicStroke, Color, Font, Graphics2D}

import Vqs.QReg

case class GraphCanvas(w: Int=500, h: Int=500) {

  var size : (Int, Int) = (w, h)
  val canvas = new BufferedImage(size._1, size._2, BufferedImage.TYPE_INT_RGB)

  var g = canvas.createGraphics()

  var hotRed     = new Color(255,100,80)
  var paleRed    = new Color(150, 0,0)
  var hotGreen   = new Color(50,255,50)
  var paleGreen  = new Color(0, 120, 0)
  var black      = Color.BLACK
  var magenta    = new Color(150,0,150)
  var palemagenta= new Color(50,5,50)
  var paleblue   = new Color(0,0,180)


  val GreenColoringScheme = 0
  val RedColoringScheme   = 1
  val JumpColoringScheme  = 2

  // clear background
  g.setColor(Color.BLACK)
  g.fillRect(0, 0, canvas.getWidth, canvas.getHeight)

  // g.setRenderingHint(java.awt.RenderingHints.KEY_ANTIALIASING, java.awt.RenderingHints.VALUE_ANTIALIAS_ON)

  def save(fn : String): Unit = {
    g.dispose()
    javax.imageio.ImageIO.write(canvas, "png", new java.io.File("vqsimages/"+fn+".png"))
  } // save


  def drawFilledCircle(x: Int, y: Int, r: Int,   c: Color = Color.BLACK): Unit = {
    // draw two filled circles
    g.setColor(c)
    g.fill(new Ellipse2D.Double(x, y, r, r))
  } // drawFillesCircle


  def drawCircle(x: Int, y: Int, r: Int,   c: Color = Color.BLACK): Unit = {
    // draw two filled circles
    g.setColor(c)
    g.draw(new Ellipse2D.Double(x, y, r, r))
  } // drawFillesCircle



  def drawFilledRectangle(x: Int, y: Int, w: Int, h: Int,   c: Color = Color.BLACK): Unit = {
    g.setColor(c)
    g.fillRect(x, y, w,h)
  } // drawFillesCircle


  def drawArc(x: Int, y: Int, r: Int, start: Int, extend: Int, c: Color = Color.BLACK): Unit = {
    g.setColor(c)
    g.draw(new Arc2D.Double(x,y,r,r,start,extend,0))
  } // drawFillesCircle

  def drawLine(x: Int, y: Int, z: Int, t: Int, c: Color= Color.BLACK): Unit = {
    g.setStroke(new BasicStroke()) // reset to default
    g.setColor(c)
    g.draw(new Line2D.Double(x,y, z, t))
  } // drawLine

  def drawText(text: String, x: Int, y: Int, f: Font = new Font("Calibri", Font.PLAIN, 20), c : Color= Color.BLACK): Unit = {
    g.setColor(c)
    g.setFont(f)
    g.drawString(text, x, y)
  } // drawText



  def drawState(x : Int, y : Int,
                amplitude: Double, phase_ : Double,
                osize: Int =20,
                v_ : Int,
                nbm: Int,
                lchanged : List[Int] = List(),
                lcond: List[Int] = List(),
                qbState : List[Int] = List(),
                qb0 : List[Int] = List(),
                qb1 : List[Int] = List(),
                isAnticlock_ : Boolean = false,
                coloringScheme : Int = GreenColoringScheme,
                coloringPow : Int = 3
               ): Unit = { // draws a state as a picture

    var phase: Double = if (isAnticlock_) -phase_ else phase_
    var ante = false;
    var red = paleRed;
    var green = paleGreen;
    val s = 4
    var exclude = false

    var nb = if (nbm < 9) nbm else if (nbm <= 0) 1 else nbm;

    (0 until nb).reverse.foreach(
      v => {
        ante = true // false for real

        if (nb > 1) {
          if (lchanged.contains(v)) {
            red = hotRed;
            green = hotGreen;
          } else {
            red = paleRed;
            green = paleGreen
          }
        } else {
          red = hotRed;
          green = hotGreen;
        }



        if (nb > 1) { // coloring scheme for multiple circles
          val v1 = v + 1
          if ((v_ & (math.pow(2, v).toInt)) == 0) {
            if (ante || (v == 0))
              drawFilledCircle(x - (osize) - v1 * s, y - (osize) - v1 * s, osize * s / 2 + v1 * s * 2, red)
            if (qb1.contains(v)) exclude = true
          } else {
            ante = true
            drawFilledCircle(x - (osize) - v1 * s, y - (osize) - v1 * s, osize * s / 2 + v1 * s * 2, green)
            if (qb0.contains(v)) exclude = true
          }
          drawCircle(x - (osize) - v1 * s, y - (osize) - v1 * s, osize * s / 2 + v1 * s * 2, new Color(0, 0, 0))
        } else { // coloring Scheme for mono circle
          val v1 = v + 1

           val res1 = if (qb1.size >0)
                           qb1.map( bt1 => if ( (v_ & (math.pow(2,bt1).toInt)) > 0) 1 else 0  ).product
                       else 1

           val res2 = if (qb0.size > 0)
                           qb0.map( bt0 => if ( (~v_ & (math.pow(2,bt0).toInt) ) >0) 1 else 0).product
                      else 1

          //  print(v_ +" :"+res1+" "+res2+"\n")
          if (res1*res2 == 0) exclude = true

          coloringScheme match {

            case GreenColoringScheme => {
              drawFilledCircle(x - (osize) - v1 * s, y - (osize) - v1 * s, osize * s / 2 + v1 * s * 2, green)
            }

            case RedColoringScheme => {
              drawFilledCircle(x - (osize) - v1 * s, y - (osize) - v1 * s, osize * s / 2 + v1 * s * 2, red)
            }

            case JumpColoringScheme => { // see coloringPox
              if ((math.floor(v_ / (math.pow(2, coloringPow).toInt))).toInt % 2 == 0) {
                drawFilledCircle(x - (osize) - v1 * s, y - (osize) - v1 * s, osize * s / 2 + v1 * s * 2, green)
              } else {
                drawFilledCircle(x - (osize) - v1 * s, y - (osize) - v1 * s, osize * s / 2 + v1 * s * 2, red)
              }
            }
          }

        }

      }
    )


    val g2 = g.asInstanceOf[Graphics2D]
    g2.setPaint(new Color(255, 255, 240)); g2.setStroke(new BasicStroke(1))

    if (isAnticlock_) {
         g2.draw(new Line2D.Float(x - osize - nb*s/2, y - osize+5 - nb*s/2, x - osize +5 - nb*s/2, y - osize - nb*s/2))
    } else {
         g2.draw(new Line2D.Float(x + osize-5 + nb*s/2, y - osize - nb*s/2, x + osize + nb*s/2, y - osize+5 - nb*s/2))
    }


    if (amplitude > 1E-10)
      drawFilledCircle(x-(osize),y-(osize),osize*2, new Color(0,0,130))
    else
      drawFilledCircle(x-(osize),y-(osize),osize*2, Color.BLACK)

    // sqrt to boost the size
    val camp = (math.sqrt(amplitude)*osize).toInt
    drawFilledCircle(x-(camp),y-(camp),camp*2, new Color(0,160,250))

    val campk= (amplitude*osize).toInt
    drawFilledCircle(x-(campk),y-(campk),campk*2, new Color(0,200,250))


    val cx = (x+osize*math.cos(phase- math.Pi/2)).toInt
    val cy = (y+osize*math.sin(phase- math.Pi/2)).toInt

    // val cx2 = (x+amplitude*osize*math.cos(phase- math.Pi/2)).toInt
    // val cy2 = (y+amplitude*osize*math.sin(phase- math.Pi/2)).toInt

    if (amplitude > 1E-10) {
      import java.awt.geom.Line2D
      import java.awt.{BasicStroke, Color, Graphics2D}

      val g2 = g.asInstanceOf[Graphics2D]
      g2.setPaint(new Color(255, 255, 240))
      g2.setStroke(new BasicStroke(3))
      g2.draw(new Line2D.Float(x,y,cx,cy))

      g2.setPaint(new Color(253, 253, 253))
      g2.setStroke(new BasicStroke(1))
      g2.draw(new Line2D.Float(x,y+1,cx,cy+1))

      g2.setStroke(new BasicStroke(1))
      g2.setPaint(new Color(0, 200, 240))

    }


    (0 until nb). foreach ( // conditionals (enhance the bits used by the cond)
      v => {
        if (nb > 1) {
          if (lcond contains v) {
            drawFilledCircle(x - 3, y - (osize) - 4 - 4 * v, 6, Color.BLACK)
            if ((v_ & (math.pow(2, v).toInt)) == 0) {
              drawCircle(x - 3, y - (osize) - 4 - 4 * v, 6, Color.WHITE)
            } else {
              drawFilledCircle(x - 3, y - (osize) - 4 - 4 * v, 6, Color.WHITE)
              drawCircle(x - 3 - 1, y - (osize) - 4 - 1 - 4 * v, 6 + 1, Color.BLACK)
            }
          }
        }
      })

    if (exclude == true) // cuts the excluded states
     drawFilledRectangle(x-(osize)- 7*s,y-(osize)-7*s,osize*s/2+7*s*2,(osize*s/2+7*s*2)/2 , Color.BLACK)

  } // drawState


}
