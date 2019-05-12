package homework2

case class RegistrationForm(name: String,
                            email: String,
                            password: String,
                            passwordConfirmation: String,
                            birthYear: String,
                            birthMonth: String,
                            birthDay: String,
                            postalCode: String)

sealed trait RegistrationFormError

case object NameIsEmpty extends RegistrationFormError

case class InvalidEmail(email: String) extends RegistrationFormError

case object PasswordTooShort extends RegistrationFormError

case object PasswordRequiresGreaterSymbolVariety extends RegistrationFormError

case object PasswordsDoNotMatch extends RegistrationFormError

case class InvalidBirthdayDate(dateErrors: Chain[DateError]) extends RegistrationFormError

case class BirthdayDateIsInTheFuture(date: Date) extends RegistrationFormError

case class InvalidPostalCode(code: String) extends RegistrationFormError

sealed trait DateError

case class YearIsNotAnInteger(year: String) extends DateError

case class MonthIsNotAnInteger(month: String) extends DateError

case class DayIsNotAnInteger(day: String) extends DateError

case class MonthOutOfRange(month: Int) extends DateError

case class DayOutOfRange(day: Int) extends DateError

case class InvalidDate(date: Date) extends DateError

case class Email(user: String, domain: String)

case class User(name: String,
                email: Email,
                passwordHash: String,
                birthday: Date,
                postalCode: Option[String])

object UserRegistration {
  def registerUser(userCountryPostalCodeVerifier: String => Boolean, today: Date)
                  (form: RegistrationForm): Validated[RegistrationFormError, User] = ???

  private def validateName(name: String) = {
    if (name.isEmpty) {
      Invalid(NameIsEmpty)
    }
    else {
      Valid(name)
    }
  }

  private def validateEmail(email: String) = {
    if (email.matches("[0-9a-zA-Z-_]*@[0-9a-zA-Z-_]*")) {
      Valid(email)
    }
    else {
      Invalid(InvalidEmail(email))
    }
  }

  private def validatePassword(password: String, passwordConfirmation: String) = {

    // TODO: Implement the below functions
    def hasAtLeastOneSpecialSymbol(password: String): Boolean = ???

    def hasAtLeastOneDigit(password: String): Boolean = ???

    def hasAtLeastOneCharacter(password: String): Boolean = ???

    def passwordHasGoodSymbolVariety(password: String) =
      hasAtLeastOneCharacter(password) && hasAtLeastOneDigit(password) && hasAtLeastOneSpecialSymbol(password)

    if (password.length < 8) {
      Invalid(PasswordTooShort)
    }
    else if (!passwordHasGoodSymbolVariety(password)) {
      Invalid(PasswordRequiresGreaterSymbolVariety)
    }
    else if (password != passwordConfirmation) {
      Invalid(PasswordsDoNotMatch)
    }
    else {
      Valid(PasswordUtils.hash(password))
    }
  }

  private def isAllDigits(x: String) = x forall Character.isDigit

  private def validateBirthDay(birthDay: String) = {
    def isDayOutOfRange(birthDay: Int) = 1 <= birthDay && birthDay <= 31

    if (!isAllDigits(birthDay)) {
      Invalid(DayIsNotAnInteger)
    }
    else if (isDayOutOfRange(birthDay.toInt)) {
      Invalid(DayOutOfRange)
    }
    else {
      Valid(birthDay.toInt)
    }
  }

  private def validateBirthMonth(birthMonth: String) = {
    def isMonthOutOfRange(birthMonth: Int) = 1 <= birthMonth && birthMonth <= 12

    if (!isAllDigits(birthMonth)) {
      Invalid(MonthIsNotAnInteger)
    }
    else if (isMonthOutOfRange(birthMonth.toInt)) {
      Invalid(MonthOutOfRange)
    }
    else {
      Valid(birthMonth.toInt)
    }
  }

  private def validateBirthYear(birthYear: String) =
    if (!isAllDigits(birthYear)) {
      Invalid(YearIsNotAnInteger)
    }
    else {
      Valid(birthYear.toInt)
    }

  private def validateDate(birthDay: Int, birthMonth: Int, birthYear: Int) =
    Date.applyOption(birthDay, birthMonth, birthYear) match {
      case None => Invalid(InvalidDate(Date(birthDay, birthMonth, birthYear)))
      case Some(date) => Valid(date)
    }

  private def validateBirthDayDate(date: Date) =
    if (date.isInTheFutre) {
      Invalid(BirthdayDateIsInTheFuture)
    }
    else {
      Valid(date)
    }
}
