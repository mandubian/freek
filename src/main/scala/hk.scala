package freek

import cats.~>


trait FunctorK[T[_[_], _]] {
  def mapk[F[_], G[_]](tfa: F ~> G): T[F, ?] ~> T[G, ?]
}

trait PointedK[T[_[_], _]] {
  def purek[F[_]]: F ~> T[F, ?]
}

object PointedK {

  def apply[T[_[_], _]](implicit p: PointedK[T]): PointedK[T] = p

}

trait CoPointedK[T[_[_], _]] {
  def copurek[F[_]]: T[F, ?] ~> F
}

trait CoZipK[T[_[_], _]] {
  self =>

  def cozipk2[F[_], G[_]]: T[In2[F, G, ?], ?] ~> In2[T[F, ?], T[G, ?], ?]

  def cozipk[C[_] <: CoproductK[_]](implicit coZipper: CoZipperK[C]): T[C, ?] ~> coZipper.Out = coZipper.cozipk

  trait CoZipperK[C[_] <: CoproductK[_]] {
    type Out[_] <: CoproductK[_]

    def cozipk: T[C, ?] ~> Out
  }

  object CoZipperK {

    type Aux[C[_] <: CoproductK[_], D[_] <: CoproductK[_]] = CoZipperK[C] { type Out[t] = D[t] }

    implicit def two[F[_], G[_]] = new CoZipperK[In2[F, G, ?]] {
      type Out[t] = In2[T[F, ?], T[G, ?], t]

      val cozipk = self.cozipk2[F, G]
    }

    implicit def three[F[_], G[_], H[_]](
      implicit functork: FunctorK[T]
    ) = new CoZipperK[In3[F, G, H, ?]] {
      type Out[t] = In3[T[F, ?], T[G, ?], T[H, ?], t]

      val nat = functork.mapk(new (In3[F, G, H, ?] ~> In2[F, In2[G, H, ?], ?]) {
        def apply[A](t: In3[F, G, H, A]): In2[F, In2[G, H, ?], A] = {
          t match {
            case In3l(l) => In2l(l)
            case In3m(m) => In2r(In2l(m))
            case In3r(r) => In2r(In2r(r))
          }
        }
      })

      val cozipk = new (T[In3[F, G, H, ?], ?] ~> In3[T[F, ?], T[G, ?], T[H, ?], ?]) {
        def apply[A](t: T[In3[F, G, H, ?], A]): In3[T[F, ?], T[G, ?], T[H, ?], A] = {
          self.cozipk2[F, In2[G, H, ?]](nat(t)) match {
            case In2l(l) => In3l(l)
            case In2r(r) => self.cozipk2[G, H](r) match {
              case In2l(l) => In3m(l)
              case In2r(r) => In3r(r)
            }
          }
        }
      }
    }

    implicit def rec[L[_] <: CoproductK[_], R[_] <: CoproductK[_], LO[_] <: CoproductK[_], RO[_] <: CoproductK[_]](
      implicit
        functork: FunctorK[T]
      , nextL: CoZipperK.Aux[L, LO]
      , nextR: CoZipperK.Aux[R, RO]
    ) = new CoZipperK[AppendK[L, R, ?]] {
      type Out[t] = AppendK[LO, RO, t]

      val nat = functork.mapk(new (AppendK[L, R, ?] ~> In2[L, R, ?]) {
        def apply[A](t: AppendK[L, R, A]): In2[L, R, A] = {
          t match {
            case Aplk(l) => In2l(l)
            case Aprk(r) => In2r(r)
          }
        }
      })

      val cozipk = new (T[AppendK[L, R, ?], ?] ~> AppendK[LO, RO, ?]) {
        def apply[A](t: T[AppendK[L, R, ?], A]): AppendK[LO, RO, A] = {
          self.cozipk2[L, R](nat(t)) match {
            case In2l(l) => Aplk(nextL.cozipk(l))
            case In2r(r) => Aprk(nextR.cozipk(r))
          }
        }
      }
    }
  }

}

trait MonadK[T[_[_], _]] extends PointedK[T] with FunctorK[T] {
  def bindk[F[_]]: T[T[F, ?], ?] ~> T[F, ?]
}

trait CoMonadK[T[_[_], _]] extends CoPointedK[T] with FunctorK[T] {
  def cobindk[F[_]]: T[F, ?] ~> T[T[F, ?], ?]
}


object catk {


}