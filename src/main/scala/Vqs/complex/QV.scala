package Vqs.complex

/** manages a vector of complex numbers
 *
 *  @constructor creates a vector of complex numbers
 *  @param c_ Collection of complex as parameters
 */

case class QV(c_ : QComplex*) {

   /** values as an array (lazy)*/
   lazy val coord: Array[QComplex] = c_.toArray[QComplex]

   /** norm (lazy) */
   lazy val norm = math.sqrt(coord.map( c => (c*c.conj).re).sum)

   /** length / size of vector */
   def length= this.c_.length

   /** access to one coord value at index idx
    *  @param idx index
    */
   def apply(idx: Int)= this.coord(idx)

   /** gets the collection of complex as a list */
   def asList = this.coord.toList

   /** gets the collection of complex as an array */
   def asArray  = this.coord

   /** creates a new normalized vector */
   def normalize : QV = {
      QV(this.coord.map( _ /this.norm) : _*)
   }

   /** conjugate  */
   def conjugate : QV = {
      QV(this.coord.map( _.conj ) : _*)
   }

   /** vector as String */
   override def toString: String = this.coord.mkString("(",",\t\t",")")
}
