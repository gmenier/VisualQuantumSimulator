package Vqs.complex

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

  /** creates a new matrix using *
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

  /** as string */
  override def toString: String = this.coord.mkString("|","\n|","|")
}
