package learnfp.traversable

import learnfp.applicative.Applicative
import learnfp.foldable.Foldable
import learnfp.functor.{Functor, Id}


trait Traversable[C[_]] {
  def traverse[A, B, F[_]](xs:C[A])(fx:A => F[B])(implicit foldable: Foldable[C], functor:Functor[F], applicative: Applicative[F]):F[C[B]]
}

object TraversableInstances {
  import learnfp.foldable.FoldableInstances._
  import learnfp.applicative.ApplicativeOps._
  import learnfp.functor.FunctorOps._

  implicit val idTraversableInstance = new Traversable[Id] {
    override def traverse[A, B, F[_]](xs: Id[A])(fx:A => F[B])(implicit foldable: Foldable[Id], functor:Functor[F], applicative: Applicative[F]): F[Id[B]] = {
      fx(xs.value) fmap (Id(_))
    }
  }

  type STuple3[A] = (A, A, A)
  def stuple3[A](a:A, b:A, c:A):STuple3[A] = (a, b, c)
  def stup[A](a:A)(b:A)(c:A) = (a, b, c)

  implicit val tuple3TraversableInstance = new Traversable[STuple3] {
    override def traverse[A, B, F[_]](xs: (A, A, A))(fx: A => F[B])(implicit foldable: Foldable[STuple3], functor: Functor[F], applicative: Applicative[F]): F[(B, B, B)] = {
      stup[B] _ `<$>` fx(xs._1) <*> fx(xs._2) <*> fx(xs._3)
    }
  }

  implicit val listTraversableInstance = new Traversable[List] {
    override def traverse[A, B, F[_]](xs: List[A])(fx: A => F[B])(implicit foldable: Foldable[List],
                                                                  functor: Functor[F], applicative: Applicative[F]): F[List[B]] = {
      foldable.foldr (xs) (applicative.pure(List[B]())) ((r, fxs) => {
        fx(r).fmap(b => {(x:List[B]) => x.+:(b)}) <*> fxs
      })
    }
  }
}

class TraversableOps[A, C[_]](xs:C[A])(implicit foldable: Foldable[C], traversable: Traversable[C]) {
  def traverse[B, F[_]](fx:A=>F[B])(implicit functor: Functor[F], applicative: Applicative[F]):F[C[B]] = traversable.traverse(xs)(fx)
}

object TraversableOps {
  implicit def toTraversableOps[A, C[_]](xs:C[A])(implicit foldable:Foldable[C], traversable: Traversable[C]) = new TraversableOps[A, C](xs)
}

class SequenceOps[A, C[_], F[_]](xs:C[F[A]])(implicit foldable: Foldable[C], traversable: Traversable[C],
                                             functor: Functor[F], applicative: Applicative[F]) {
  def sequence:F[C[A]] = traversable.traverse(xs)(identity)
}

object SequenceOps {
    implicit def toSequenceOps[A, C[_], F[_]](xs:C[F[A]])(implicit functor:Functor[F], applicative: Applicative[F],
                                                             foldable:Foldable[C], traversable: Traversable[C]) = new SequenceOps[A, C, F](xs)
}


