package homework2

sealed trait Chain[+A] {
  def head: A

  def tail: Option[Chain[A]]

  def isEmpty: Boolean = this match {
    case Chain() => true
    case _ => false
  }

  def +:[B >: A](front: B): Chain[B] = ???

  def :+[B >: A](back: B): Chain[B] = ???

  def ++[B >: A](right: Chain[B]): Chain[B] = Append(this, right)

  private def foldLeftListified[B](initial: B)(f: (B, A) => B): B = ???

  def foldLeft[B](initial: B)(f: (B, A) => B): B = {
    val thisListified = this.listify

    if (isEmpty) {
      
    }
    thisListified match {
      case Singleton(elem) => initial
      case Append(Singleton(elem), rest) => f(initial, elem)
    }
  }

  def reduceLeft[B >: A](f: (B, A) => B): B = this.listify match {
    case Singleton(first) => first
    case Append(Singleton(first), rest) => rest.foldLeft(first: B)(f)
    case _ => sys.error("Unexpected listify format")
  }

  def map[B](f: A => B): Chain[B] = ???

  def flatMap[B](f: A => Chain[B]): Chain[B] = ???

  def foreach(f: A => Unit): Unit = foldLeft(())((_, next) => f(next))

  override def equals(that: Any): Boolean = that match {
    case c: Chain[_] => this.hashCode == c.hashCode
    case _ => false
  }

  override def hashCode: Int = foldLeft(0) {
    _ * 31 + _.hashCode
  }

  override def toString: String = toList.mkString("Chain(", ",", ")")

  def toList: List[A] = foldLeft(List.empty[A])((acc, next) => next :: acc).reverse

  def toSet[B >: A]: Set[B] = foldLeft(Set.empty[B])((acc, next) => acc + next)

  def min[B >: A](implicit order: Ordering[B]): B = ???

  def max[B >: A](implicit order: Ordering[B]): B = ???

  def listify: Chain[A] = {
    if (isEmpty) {
      this
    }
    else {
      this match {
        case Singleton(_) => this
        case Append(Singleton(_), Singleton(_)) => this
        case Append(Singleton(elem), right) => Append(Singleton(elem), right.listify)
        case Append(Append(l, r), right) => Append(l.listify, Append(r.listify, right.listify)).listify
      }
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

object MyTest {
  def main(args: Array[String]): Unit = {
    println("Hello, world!")
  }
}
