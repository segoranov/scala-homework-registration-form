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
                  (form: RegistrationForm): Validated[RegistrationFormError, User] = ???/*{
    (
      validatePostalCode(userCountryPostalCodeVerifier, form.postalCode),
      validateEmail(form.email),
      validatePassword(form.password, form.passwordConfirmation),
      validateName(form.name),
      validateBirthDayDate(form.birthDay, form.birthMonth, form.birthYear)
    ).zipMap(User.apply)
  }*/

  private[homework2] def validatePostalCode(userCountryPostalCodeVerifier: String => Boolean)(postalCode: String) = {
    if (userCountryPostalCodeVerifier(postalCode)) {
      Valid(postalCode)
    }
    else {
      Invalid(InvalidPostalCode(postalCode))
    }
  }

  private[homework2] def validateName(name: String) = {
    if (name.isEmpty) {
      Invalid(NameIsEmpty)
    }
    else {
      Valid(name)
    }
  }

  private[homework2] def validateEmail(email: String) = {
    if (email.matches("[0-9a-zA-Z-_]*@[0-9a-zA-Z-_]*")) {
      Valid(email)
    }
    else {
      Invalid(InvalidEmail(email))
    }
  }

  private[homework2] def validatePassword(password: String, passwordConfirmation: String) = {

    // -----------------
    def validatePasswordHasGoodSymbolVariety(password: String) = {
      def passwordHasGoodSymbolVariety(password: String) = {
        // TODO: Implement the below functions
        def hasAtLeastOneSpecialSymbol(password: String): Boolean = ???

        def hasAtLeastOneDigit(password: String): Boolean = ???

        def hasAtLeastOneCharacter(password: String): Boolean = ???

        hasAtLeastOneCharacter(password) && hasAtLeastOneDigit(password) && hasAtLeastOneSpecialSymbol(password)
      }

      if (passwordHasGoodSymbolVariety(password)) {
        Valid(password)
      }
      else {
        Invalid(PasswordRequiresGreaterSymbolVariety)
      }
    }

    // -----------------
    def validatePasswordsMatching(password: String, passwordConfirmation: String) = {
      if (password == passwordConfirmation) {
        Valid(password)
      }
      else {
        Invalid(PasswordsDoNotMatch)
      }
    }

    // -----------------
    def validatePasswordLength(password: String) = {
      if (password.length >= 8) {
        Valid(password)
      }
      else {
        Invalid(PasswordTooShort)
      }
    }

    // -----------------
    (validatePasswordHasGoodSymbolVariety(password),
      validatePasswordLength(password),
      validatePasswordsMatching(password, passwordConfirmation)).zip
  }


  private[homework2] def validateBirthDayDate(birthDay: String, birthMonth: String, birthYear: String) = {
    def isAllDigits(x: String) = x forall Character.isDigit

    // ----------------------------------
    def validateBirthDay(birthDay: String) = {
      def validateDayIsAnInteger(birthDay: String) = {
        if (!isAllDigits(birthDay)) {
          Invalid(DayIsNotAnInteger(birthDay))
        }
        else {
          Valid(birthDay.toInt)
        }
      }

      // -----------------
      def validateDayIsInRange(birthDay: Int) = {
        if (1 <= birthDay && birthDay <= 31) {
          Valid(birthDay)
        }
        else {
          Invalid(DayOutOfRange(birthDay))
        }
      }

      // -----------------
      validateDayIsAnInteger(birthDay) match {
        case i@Invalid(_) => i
        case Valid(day) => validateDayIsInRange(day) match {
          case i@Invalid(_) => i
          case v@Valid(_) => v
        }
      }
    }

    // ----------------------------------
    def validateBirthMonth(birthMonth: String) = {
      def validateMonthIsAnInteger(birthMonth: String) = {
        if (!isAllDigits(birthMonth)) {
          Invalid(MonthIsNotAnInteger(birthMonth))
        }
        else {
          Valid(birthMonth.toInt)
        }
      }

      // -----------------
      def validateMonthIsInRange(birthMonth: Int) = {
        if (1 <= birthMonth && birthMonth <= 12) {
          Valid(birthMonth)
        }
        else {
          Invalid(MonthOutOfRange(birthMonth))
        }
      }

      // -----------------
      validateMonthIsAnInteger(birthMonth) match {
        case i@Invalid(_) => i
        case Valid(month) => validateMonthIsInRange(month) match {
          case i@Invalid(_) => i
          case v@Valid(_) => v
        }
      }
    }

    // ----------------------------------
    def validateBirthYear(birthYear: String) =
      if (!isAllDigits(birthYear)) {
        Invalid(YearIsNotAnInteger(birthYear))
      }
      else {
        Valid(birthYear.toInt)
      }

    // ----------------------------------
    def validateDate(birthDay: String, birthMonth: String, birthYear: String) = {
      (
        validateBirthDay(birthDay),
        validateBirthMonth(birthMonth),
        validateBirthYear(birthYear)
      ).zip match {
        case i@Invalid(_) => i
        case v@Valid(_) => {
          Date.applyOption(birthDay.toInt, birthMonth.toInt, birthYear.toInt) match {
            case None => Invalid(InvalidDate(Date(birthDay.toInt, birthMonth.toInt, birthYear.toInt)))
            case Some(date) => Valid(date)
          }
        }
      }
    }

    // ----------------------------------
    validateDate(birthDay, birthMonth, birthYear) match {
      case i@Invalid(_) => InvalidBirthdayDate(i.errors)
      case Valid(date) => {
        if (date.isInTheFutre) {
          Invalid(BirthdayDateIsInTheFuture(date))
        }
        else {
          Valid(date)
        }
      }
    }
  }
}
