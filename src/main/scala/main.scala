// VQS : Quantum Computing Simulation
// Gildas Ménier
// 2020
// gildas.menier@univ-ubs.fr



import Vqs._


object main {

  def main(args : Array[String]): Unit = {

    import Vqs._

    val rr = QReg(3)

    rr.pdf("teleport.pdf"); rr.trace(2)

    rr - H(1) - C(X(2), 1)
    rr - H(0) - Rz(0, 45) - H(0)
    rr - C(X(1), 0) - H(0)
    rr - <(0) - <(1)

    val alice = rr.readQbit(0)
    val ep = rr.readQbit(1)
    if (ep == 1) rr - X(2)
    if (alice == 1) rr - Z(2)

    rr - H(2) - Rz(2, -45) - H(2)
    rr - <(2)

    rr.end()



  } // main

} // Main
