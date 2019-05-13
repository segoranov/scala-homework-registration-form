package homework2

import homework2.UserRegistration.registerUser
import org.scalatest.{FlatSpec, Matchers}

class UserRegistrationTest extends FlatSpec with Matchers {
  "An empty form" should "generate errors for the non optional fields" in {
    val emptyForm = RegistrationForm("", "", "", "", "", "", "", "")

    val validation = registerUser(Set.empty, Date(2019, 5, 4))(emptyForm)

    validation.isValid shouldBe false

    val Invalid(errors) = validation
    val errorsSet = errors.toSet
    val birthdayErrors = errorsSet.collectFirst {
      case InvalidBirthdayDate(dateErrors) => dateErrors.toSet
    }

    errorsSet should have size 5

    errorsSet should contain allOf(
      NameIsEmpty,
      InvalidEmail(""),
      PasswordTooShort,
      PasswordRequiresGreaterSymbolVariety
    )

    birthdayErrors shouldEqual Some(Set(
      YearIsNotAnInteger(""),
      MonthIsNotAnInteger(""),
      DayIsNotAnInteger("")
    ))
  }

  "validation for birthdate" should "generate errors for day out of range" in {
    UserRegistration.validateBirthdayDate("2019", "11", "300") shouldBe
      Invalid(InvalidBirthdayDate(Chain(DayOutOfRange(300))))
  }

  it should "generate errors for day and month out of range" in {
    UserRegistration.validateBirthdayDate("2019", "305", "300") shouldBe
      Invalid(InvalidBirthdayDate(Chain(MonthOutOfRange(305), DayOutOfRange(300))))
  }

  it should "generate errors for day, month and year not an integers" in {
    UserRegistration.validateBirthdayDate("20f19", "3b00", "300s") shouldBe
      Invalid(InvalidBirthdayDate(
        Chain(
          YearIsNotAnInteger("20f19"),
          MonthIsNotAnInteger("3b00"),
          DayIsNotAnInteger("300s"))))
  }

  it should "generate error for non-existing date" in {
    // year 2013 was not a leap year, hence february has only 28 days
    UserRegistration.validateBirthdayDate("2013", "02", "29") shouldBe
      Invalid(InvalidBirthdayDate(Chain(InvalidDate(Date(2013, 2, 29)))))
  }

  it should "generate error for birthday in the future" in {
    UserRegistration.validateBirthdayDate("3000", "06", "06") shouldBe
      Invalid(BirthdayDateIsInTheFuture(Date(3000, 6, 6)))
  }

  it should "generate valid birhday" in {
    UserRegistration.validateBirthdayDate("2013", "02", "28") shouldBe
      Valid(Date(2013, 2, 28))
  }

  "validation for password" should "generate error for greater symbol variery required" in {
    UserRegistration.validatePassword("12345678", "12345678") shouldBe
      Invalid(PasswordRequiresGreaterSymbolVariety)
  }

  it should "generate errors for greater symbol variety and not enough length" in {
    UserRegistration.validatePassword("1234567", "1234567") shouldBe
      Invalid(Chain(PasswordTooShort, PasswordRequiresGreaterSymbolVariety))
  }

  it should "generate all possible errors for password" in {
    UserRegistration.validatePassword("1234567", "123456") shouldBe
      Invalid(Chain(PasswordsDoNotMatch,PasswordTooShort,PasswordRequiresGreaterSymbolVariety))
  }

  it should "generate valid password" in {
    UserRegistration.validatePassword("1234567abcde!", "1234567abcde!") shouldBe
      Valid("1234567abcde!")
  }

  it should "generate error for passwords do not match" in {
    UserRegistration.validatePassword("1234567abcd!e", "1234567abcde!") shouldBe
      Invalid(PasswordsDoNotMatch)
  }

  it should "generate error for password too short" in {
    UserRegistration.validatePassword("12ab!", "12ab!") shouldBe
      Invalid(PasswordTooShort)
  }

  it should "generate errors for password too short and passwords not matching" in {
    UserRegistration.validatePassword("12ab!", "12a!") shouldBe
      Invalid(Chain(PasswordsDoNotMatch, PasswordTooShort))
  }
}
