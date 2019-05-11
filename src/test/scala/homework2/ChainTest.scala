package homework2

import org.scalatest.{FlatSpec, Matchers}

class ChainTest extends FlatSpec with Matchers {

  def isListified[A](c: Chain[A]): Boolean = c match {
    case Singleton(_) => true
    case Append(Singleton(_), rest) => isListified(rest)
    case _ => false
  }

  val testChain = Chain(1, 2, 3) ++ Chain(4, 5, 6) ++ Chain(7, 8, 9) ++ Chain(10)

  "head" should "be 5" in {
    Append(Chain(5, 1), Chain(5, 1)).head shouldEqual 5
  }

  "equals" should "work correctly" in {
    testChain shouldBe testChain

    Append(Singleton(1), Append(Singleton(2), Append(Singleton(3), Singleton(4)))) shouldBe
      Append(Append(Singleton(1), Singleton(2)), Append(Singleton(3), Singleton(4)))

    Chain(1, 4, 5, 2) should not be Chain(1, 5, 4, 2)

    Chain(1, 2, 3, 4) should not be Chain(1, 2, 5, 3, 4)

    Chain(1, 2) ++ Chain(3, 4) shouldBe Chain(1, 2, 3) ++ Chain(4)
  }

  "listify" should "produce proper list-like structure" in {
    // base cases
    isListified(Singleton(1)) shouldBe true

    isListified(Append(Singleton(1), Singleton(2)).listify) shouldBe true

    // non-base cases
    isListified(
      Append(Append(Singleton(1), Singleton(2)), Append(Singleton(3), Singleton(4))).listify) shouldBe true

    isListified(
      Append(Append(Singleton(1), Singleton(2)), Append(Singleton(3), Singleton(4)))) shouldBe false

    isListified(Append(Append(Singleton(1), Append(Singleton(2), Singleton(3))),
      Append(Singleton(4), Singleton(5))).listify) shouldBe true

    isListified(Append(Append(Singleton(1), Append(Singleton(2), Singleton(3))),
      Append(Singleton(4), Singleton(5)))) shouldBe false
  }

  "++" should "append two chains" in {
    (Chain(1, 2) ++ Chain(3, 4)) shouldEqual Chain(1, 2, 3, 4)
  }

  "foldLeft" should "generate the number 55" in {
    testChain.foldLeft(0)(_ + _) shouldBe 55
  }

  "map" should "increase each number with 1" in {
    testChain.map(_ + 1) shouldBe Chain(2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
  }

  it should "produce chain of chains" in {
    testChain.map(elem => Chain(elem)) shouldBe
      Chain(Chain(1), Chain(2), Chain(3), Chain(4), Chain(5), Chain(6), Chain(7), Chain(8), Chain(9), Chain(10))
  }

  "+:" should "add element at the beginning of the chain" in {
    testChain.+:(69) shouldBe Chain(69, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  }

  ":+" should "add element at the end of the chain" in {
    testChain.:+(69) shouldBe Chain(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 69)
  }

  "min" should "be 1" in {
    testChain.min shouldBe 1
  }

  "max" should "be 10" in {
    testChain.max shouldBe 10
  }

  "flatMap" should "produce Chain(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)" in {
    Chain(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).flatMap(elem => Chain(elem)) shouldBe
      Chain(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  }
}
