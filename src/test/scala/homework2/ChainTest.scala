package homework2

import org.scalatest.{FlatSpec, Matchers}

class ChainTest extends FlatSpec with Matchers {

  "head" should "be 5" in {
    Append(Chain(5, 1), Chain(5, 1)).head shouldEqual 5
  }

  "listify" should "produce list-like structure" in {
    {
      Append(Append(Singleton(1), Singleton(2)), Append(Singleton(3), Singleton(4))).listify shouldBe
        Append(Singleton(1), Append(Singleton(2), Append(Singleton(3), Singleton(4))))
    }
  }

  //  "++" should "append two chains" in {
  //    (Chain(1, 2) ++ Chain(3, 4)) shouldEqual Chain(1, 2, 3, 4)
  //  }
}
