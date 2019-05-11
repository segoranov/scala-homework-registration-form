package homework2

import org.scalatest.{FlatSpec, FunSuite, Matchers}

class ValidatedTest extends FlatSpec with Matchers {
  "zip" should "combine valid instances" in {
    Valid(1).zip(Valid("a")) shouldEqual Valid((1, "a"))
  }

  it should "combine errors from invalid instances" in {
    Invalid(1).zip(Invalid(Chain(2, 3))) shouldEqual Invalid(Chain(1, 2, 3))
  }

  it should "return errors from the invalid instance" in {
    Valid(1).zip(Invalid(Chain(2, 3))) shouldEqual Invalid(Chain(2, 3))

    Invalid(Chain(2, 3)).zip(Valid(1)) shouldEqual Invalid(Chain(2, 3))
  }

  "getOrElse" should "return default value" in {
    Invalid(45).getOrElse(69) shouldBe 69
  }

  it should "return the valid value" in {
    Valid(45).getOrElse(69) shouldBe 45
  }

  "orElse" should "return default value" in {
    Invalid(45).orElse(Valid(69)) shouldBe Valid(69)
  }

  it should "return the object itself" in {
    Valid(45).orElse(Valid(69)) shouldBe Valid(45)
  }

  "map2" should "return Valid(3)" in {
    Valid(1).map2(Valid(2))(_ + _) shouldBe Valid(3)
  }

  it should "return Invalid(Chain(42))" in {
    Valid(1).map2(Invalid(Chain(42)))(_ + _) shouldBe Invalid(Chain(42))
  }
}
