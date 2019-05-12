package homework2

sealed trait Validated[+E, +A] {
  def isValid: Boolean = this match {
    case Valid(_) => true
    case _ => false
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Valid(a) => a
    case _ => default
  }

  def get[B >: A] = this match {
    case Valid(a) => a
    case _ => throw new IllegalAccessError("Called get on invalid Validated!")
  }

  def orElse[F >: E, B >: A](default: => Validated[F, B]): Validated[F, B] = if (isValid) this else default

  def zip[EE >: E, B](vb: Validated[EE, B]): Validated[EE, (A, B)] = {
    this match {
      case Valid(a) => vb match {
        case Valid(b) => Valid(a, b)
        case Invalid(vbErrors) => Invalid(vbErrors)
      }
      case Invalid(thisErrors) => vb match {
        case Valid(_) => Invalid(thisErrors)
        case Invalid(vbErrors) => Invalid((thisErrors ++ vbErrors).listify)
      }
    }
  }

  def map[B](f: A => B): Validated[E, B] = this match {
    case Valid(a) => Valid(f(a))
    case Invalid(errors) => Invalid(errors)
  }

  def map2[EE >: E, B, R](vb: Validated[EE, B])(f: (A, B) => R): Validated[EE, R] = this.zip(vb) match {
    case Valid(pair) => Valid(f.tupled(pair))
    case Invalid(errors) => Invalid(errors)
  }

  def flatMap[EE >: E, B](f: A => Validated[EE, B]): Validated[EE, B] = this.map(f) match {
    case Valid(Valid(a)) => Valid(a)
    case Invalid(errors) => Invalid(errors)
    case Valid(Invalid(a)) => Invalid(a)
  }

  def fold[B](invalid: Chain[E] => B, valid: A => B): B = this match {
    case Valid(a) => valid(a)
    case Invalid(errors) => invalid(errors)
  }

  def foreach(f: A => Unit): Unit = fold(_ => (), f)
}

case class Valid[+A](a: A) extends Validated[Nothing, A]

case class Invalid[+E](errors: Chain[E]) extends Validated[E, Nothing]

object Invalid {
  def apply[E](error: E): Invalid[E] = Invalid(Chain(error))
}

object Validated {

  import shapeless._
  import ops.tuple.FlatMapper

  trait LowPriorityFlatten extends Poly1 {
    implicit def default[T] = at[T](Tuple1(_))
  }

  // Used to flatten tuples. Example:
  //  val t1 = (1, ((2, 3), 4))
  //  val f1 = flatten(t1) // Inferred type is (Int, Int, Int, Int)
  object flatten extends LowPriorityFlatten {
    implicit def caseTuple[P <: Product](implicit lfm: Lazy[FlatMapper[P, flatten.type]]) =
      at[P](lfm.value(_))
  }

  def sequence[E, A](xs: List[Validated[E, A]]): Validated[E, List[A]] = ???

  implicit class ValidatedTuple2[EE, A, B](val tuple: (Validated[EE, A], Validated[EE, B])) extends AnyVal {
    def zip: Validated[EE, (A, B)] = tuple._1.zip(tuple._2)

    def zipMap[R](f: (A, B) => R): Validated[EE, R] = zip match {
      case Invalid(errors) => Invalid(errors)
      case Valid(t) => Valid(f.tupled(t))
    }
  }

  implicit class ValidatedTuple3[EE, A, B, C](val tuple: (Validated[EE, A], Validated[EE, B], Validated[EE, C])) extends AnyVal {
    def zip: Validated[EE, (A, B, C)] = ??? /*{
      val iterableTuple = tuple.productIterator.map(_.asInstanceOf[Validated[EE, Any]])
      if (iterableTuple.exists(!_.isValid)) {
        // we need all Invalids and we have to ignore the Valids
        val onlyInvalids = iterableTuple.flatMap {
          case _: Valid[A] | _: Valid[B] | _: Valid[C] => None
          case other => Some(other)
        }.map(_.asInstanceOf[Invalid[EE]])

        Invalid(onlyInvalids.reduceLeft(_.errors ++ _.errors))
      }
      else {
        // there are no Invalids, hence calling 'get' on each is safe
        Valid((tuple._1.get, tuple._2.get, tuple._3.get))
      }
    }*/

    def zipMap[R](f: (A, B, C) => R): Validated[EE, R] = ???
  }

  implicit class ValidatedTuple4[EE, A, B, C, D]
  (val tuple: (Validated[EE, A], Validated[EE, B], Validated[EE, C], Validated[EE, D])) extends AnyVal {
    def zip: Validated[EE, (A, B, C, D)] = ???

    def zipMap[R](f: (A, B, C, D) => R): Validated[EE, R] = ???
  }

  implicit class ValidatedTuple5[EE, A, B, C, D, E]
  (val tuple: (Validated[EE, A], Validated[EE, B], Validated[EE, C], Validated[EE, D], Validated[EE, E])) extends AnyVal {
    def zip: Validated[EE, (A, B, C, D, E)] = ???

    def zipMap[R](f: (A, B, C, D, E) => R): Validated[EE, R] = ???
  }

  // ??? TODO: Add toValidated to Option instances
}
