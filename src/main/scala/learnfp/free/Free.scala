package learnfp.free

import learnfp.monad.{Monad, MonadOps}
import learnfp.functor.{Functor, FunctorOps}

import scala.annotation.tailrec

sealed trait Free[F[_], A]
final case class Return[F[_], A](a:A) extends Free[F, A]
final case class FlatMap[F[_], E, A](a:Free[F, E], fx:E => Free[F, A]) extends Free[F, A]
final case class LiftF[F[_], A](fn:F[A]) extends Free[F, A]

abstract class Natural[F[_], G[_]] {
  def transform[A](a:F[A]):G[A]
}

object Free {
  implicit def freeFunctorInstance[F[_]] = new Functor[({type E[X] = Free[F, X]})#E] {
    override def fmap[A, B](a: Free[F, A])(fx: A => B): Free[F, B] = a match {
      case Return(x) => Return(fx(x))
      case FlatMap(x, gx) => FlatMap(x, ((e: Free[F, A]) => fmap(e)(fx)) compose gx)
      case l@LiftF(_) => FlatMap(l, ((e: B) => Return[F, B](e)) compose fx)
    }
  }

  implicit def freeToFunctorOps[F[_], A](a:Free[F, A]) = new FunctorOps[A, ({type E[X] = Free[F, X]})#E](a)

  implicit def freeMonadInstance[F[_]] = new Monad[({type E[X] = Free[F, X]})#E] {
    override def pure[A](a: A): Free[F, A] = Return(a)
    override def flatMap[E, A](a: Free[F, E])(fx: E => Free[F, A]): Free[F, A] = FlatMap(a, fx)
  }

  implicit def freeToMonadOps[F[_], A](a:Free[F, A]) = new MonadOps[A, ({type E[X] = Free[F, X]})#E](a)

  def liftF[F[_], A](a:F[A]):Free[F, A] = LiftF[F, A](a)

  def foldF[F[_], M[_], A](a:Free[F, A])(trans:Natural[F, M])(implicit f:Functor[M], m:Monad[M]):M[A] = a match {
    case Return(x) => m.pure(x)
    case LiftF(fa) => trans.transform(fa)
    case FlatMap(x, gx) => m.flatMap(foldF(x)(trans))(((e: Free[F, A]) => foldF(e)(trans)) compose gx)
  }
}