package homework2

sealed trait Chain[+A] {
  def head: A

  def tail: Option[Chain[A]]

  def isEmpty: Boolean = false

  def +:[B >: A](front: B): Chain[B] = Append(Singleton(front), this)

  def :+[B >: A](back: B): Chain[B] = Append(this, Singleton(back))

  def ++[B >: A](right: Chain[B]): Chain[B] = Append(this, right)

  def foldLeft[B](initial: B)(f: (B, A) => B): B = this.listify match {
    case Singleton(first) => f(initial, first)
    case Append(Singleton(first), rest) => rest.foldLeft(f(initial, first))(f)
    case _ => sys.error("Unexpected listify format")
  }

  def reduceLeft[B >: A](f: (B, A) => B): B = this.listify match {
    case Singleton(first) => first
    case Append(Singleton(first), rest) => rest.foldLeft(first: B)(f)
    case _ => sys.error("Unexpected listify format")
  }

  def map[B](f: A => B): Chain[B] = this.listify match {
    case Singleton(first) => Singleton(f(first))
    case Append(Singleton(first), rest) => Append(Singleton(f(first)), rest.map(f))
    case _ => sys.error("Unexpected listify format")
  }

  def flatMap[B](f: A => Chain[B]): Chain[B] = this.map(f).reduceLeft(_ ++ _)

  def foreach(f: A => Unit): Unit = foldLeft(())((_, next) => f(next))

  override def equals(that: Any): Boolean = that match {
    case c: Chain[A] => this.toList == c.toList
    case _ => false
  }

  override def hashCode: Int = foldLeft(0) {
    _ * 31 + _.hashCode
  }

  override def toString: String = toList.mkString("Chain(", ",", ")")

  def toList: List[A] = foldLeft(List.empty[A])((acc, next) => next :: acc).reverse

  def toSet[B >: A]: Set[B] = foldLeft(Set.empty[B])((acc, next) => acc + next)

  def min[B >: A](implicit order: Ordering[B]): B = reduceLeft[B](order.min(_, _))

  def max[B >: A](implicit order: Ordering[B]): B = reduceLeft[B](order.max(_, _))

  def listify: Chain[A] = {
    this match {
      case Singleton(_) => this
      case Append(Singleton(first), right) => Append(Singleton(first), right.listify)
      case Append(Append(left, right1), right2) =>
        Append(left.listify, Append(right1.listify, right2.listify)).listify
    }
  }
}

case class Singleton[+A](head: A) extends Chain[A] {
  def tail: Option[Chain[A]] = None
}

case class Append[+A](left: Chain[A], right: Chain[A]) extends Chain[A] {
  def head: A = left.head

  def tail: Option[Chain[A]] = left match {
    case Singleton(_) => Some(right)
    case _ => listify.tail
  }
}

object Chain {
  def apply[A](head: A, rest: A*): Chain[A] = rest match {
    case Seq() => Singleton(head)
    case Seq(elem) => Append(Singleton(head), Singleton(elem))
    case Seq(elem, elems@_*) => Append(Singleton(head), apply(elem, elems: _*))
  }

  // Allows Chain to be used in pattern matching
  def unapplySeq[A](chain: Chain[A]): Option[Seq[A]] = Some(chain.toList)
}
