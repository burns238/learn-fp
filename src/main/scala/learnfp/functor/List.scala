package learnfp.functor

import scala.annotation.tailrec

object ListInstance {
  implicit val listInstance:Functor[List] = new Functor[List] {
    override def fmap[A, B](a: List[A])(fx: A => B): List[B] = {
      a match {
        case Nil => Nil
        case head :: tail => fx(head) :: fmap(tail)(fx)
      }
    }
  }
}
