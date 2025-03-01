package learnfp.monoid

object SimpleMonoid {
  case class Sum(value:Int)
  implicit val sumMonoidInstance:Monoid[Sum] = new Monoid[Sum] {
    override def mzero: Sum = Sum(0)
    override def mappend(lhs: Sum, rhs: Sum): Sum = Sum(lhs.value + rhs.value)
  }

  case class Product(value:Int)
  implicit val productMonoidInstance:Monoid[Product] = new Monoid[Product] {
    override def mzero: Product = Product(0)
    override def mappend(lhs: Product, rhs: Product): Product = (lhs, rhs) match {
      case (left, right) if left == mzero => right
      case (left, right) if right == mzero => left
      case (left, right) => Product(left.value * right.value)
    }
  }
}
