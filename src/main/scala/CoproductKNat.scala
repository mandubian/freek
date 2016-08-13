package freek

import cats.~>

trait IsoNat[C[_] <: CoproductK[_]] {

  def replace[F[_], G[_], D[_] <: CoproductK[_]](nat: F ~> G)(
    implicit replaceF: Replace.Aux[C, F, G, D]
  ): C ~> D = new (C ~> D) {
    def apply[A](ca: C[A]): D[A] = replaceF.replace(ca)(nat)
  }

}

object IsoNat {
  def apply[C[_] <: CoproductK[_]] = new IsoNat[C] {}
}
