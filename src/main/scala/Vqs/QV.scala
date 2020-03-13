package Vqs
// QSim : simulateur quantique
// Gildas Ménier
// 2010

import QComplex._

// Vecteur complexe

case class QV(c_ : QComplex*) {
   lazy val coord: Array[QComplex] = c_.toArray[QComplex]
   lazy val norm = math.sqrt(getList.map( c => (c*c.conj).re).sum)

   def length()= coord.length
   def apply(a: Int)= coord(a)
   def getList = coord.toList
   def getArray  = coord

   def normalize : QV = { // normalize les probabilités
      QV(coord.map( v => v/norm) : _*)
   }

  override def toString: String = this.coord.mkString("(",",\t\t",")")
}


