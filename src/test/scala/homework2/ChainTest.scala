package homework2

import org.scalatest.{FlatSpec, Matchers}

class ChainTest extends FlatSpec with Matchers {

  val testChain = Chain(1, 2, 3) ++ Chain(4, 5, 6) ++ Chain(7, 8, 9) ++ Chain(10)

  "head" should "be 5" in {
    Append(Chain(5, 1), Chain(5, 1)).head shouldEqual 5
  }

  "equals" should "return true if chains are the same but their structures are different" in {
    Append(Append(Singleton(1), Singleton(2)), Append(Singleton(3), Singleton(4))) shouldBe
      Append(Singleton(1), Append(Singleton(2), Append(Singleton(3), Singleton(4))))

    Append(Append(Append(Singleton(1), Singleton(2)), Singleton(3)), Singleton(4)) shouldBe
      Append(Singleton(1), Append(Singleton(2), Append(Singleton(3), Singleton(4))))
  }

  "listify" should "produce proper list-like structure" in {
    // base cases
    Singleton(1).listify shouldBe Singleton(1)

    Append(Singleton(1), Singleton(2)).listify shouldBe Append(Singleton(1), Singleton(2))

    // non-base cases
    Append(Append(Singleton(1), Singleton(2)), Append(Singleton(3), Singleton(4))).listify shouldBe
      Append(Singleton(1), Append(Singleton(2), Append(Singleton(3), Singleton(4))))

    Append(Append(Singleton(1), Append(Singleton(2), Singleton(3))), Append(Singleton(4), Singleton(5))).listify shouldBe
      Append(Singleton(1), Append(Singleton(2), Append(Singleton(3), Append(Singleton(4), Singleton(5)))))
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
}
