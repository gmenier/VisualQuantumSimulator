package Vqs.complex

import Vqs.complex.QComplex.i

import scala.math.{cos, sin}

/** complex matrix (dense)
 *
 *  @constructor creates a matrix with complex
 *  @param l_ collection (params) of complex vectors
 */

case class QM(l_ : QV*) {

  lazy val coord: Array[QV] = l_.toArray[QV]

  /** returns a line */
  def apply(idx: Int) = coord(idx)

  /** number of lines */
  def length = coord.length

  def rowDim = length

  def colDim = if (rowDim >0) this(0).length else 0

  def v(row : Int, col : Int): QComplex =  this(row)(col)



  /** applies a matrix op
   *  @param v_ mult by v_
   */
  def mult(v_ : QV): QV = {
    val nv = for( ligne <- 0 until length) yield {
      (0 until length).map(a => coord(ligne)(a)).zip(v_.asArray).map(
        c => c._1 * c._2
      ).reduceLeft( (a,b) => a+b)
    }
    QV( nv : _*)
  } // mult



  /** tensor product
   *
   *  */
  def tensorProduct(m_ : QM): QM = { // quick'nd dirty
    var tr = Array.ofDim[QComplex](this.rowDim*m_.rowDim, this.colDim*m_.colDim)
    for (j <- 0 until this.rowDim * m_.rowDim; k <- 0 until this.colDim * m_.colDim) {
        tr(j)(k) =  this.v( j / m_.rowDim, k / m_.colDim)*m_.v( j % m_.rowDim, k % m_.colDim)
    }
    val result = tr.map( QV( _ : _*) ).toList
    QM( result : _*)
  } // tensorProduct

  /** matrix product
   *
   *  */
  def product(m_ : QM): QM = { // quick'nd dirty
    var tr = Array.ofDim[QComplex](this.rowDim, m_.colDim)

    for (j <- 0 until this.rowDim) {
      for (k <- 0 until this.colDim) {
        tr(j)(k)= QComplex(0,0)
        for(m <- 0 until m_.rowDim) {
          tr(j)(k) = tr(j)(k) + this.v(j,m)*m_.v(m,k)
        }
      }
    }
    val result = tr.map( QV( _ : _*) ).toList
    QM( result : _*)
  } // tensorProduct



  /** transpose */
  def transpose : QM = {
    var tr = Array.ofDim[QComplex](this.colDim, this.rowDim)
    for(j <- 0 until this.colDim; k <- 0 until this.rowDim) {
      tr(j)(k) = this.v(k,j)
    }
    val result = tr.map( QV( _ : _*) ).toList
    QM( result : _*)
  } // transpose

  /** conjugate */
  def conjuguate : QM = {
    QM( this.coord.map( _.conjugate) : _*)
  } // conjugate

  /** adj */
  def adjoint : QM = {
    QM( this.coord.map( _.conjugate) : _*).transpose
  } // conjugate


  def <<(M: QM):QM = {
    M.adjoint
  }

  def >>: (M:QM):QM = {
    M
  }

  def |: (B: QM) = {
    B.tensorProduct(this)
  }
  /**
   * as string */
  override def toString: String = this.coord.mkString("|","\n|","|")
}
