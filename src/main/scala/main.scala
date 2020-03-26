// VQS : Quantum Computing Simulation
// Gildas Ménier
// 2020
// gildas.menier@univ-ubs.fr






object main {

  def main(args : Array[String]): Unit = {
    // QUtils.removeImages
    // QStudy.caseQFT()

    import Vqs._
    val rr = QReg(2)
    rr.init(2)
    rr - Swap(0,1) - <()
    println(rr.render)
    rr.end()

  } // main

} // Main
