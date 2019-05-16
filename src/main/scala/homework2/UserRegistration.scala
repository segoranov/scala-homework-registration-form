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
                  (form: RegistrationForm): Validated[RegistrationFormError, User] = {

    val validatedPassword = validatePassword(form.password, form.passwordConfirmation) match {
      case i@Invalid(_) => i
      case Valid(password) => Valid(PasswordUtils.hash(password))
    }

    (
      validateName(form.name),
      validateEmail(form.email),
      validatedPassword,
      validateBirthdayDate(form.birthYear, form.birthMonth, form.birthDay, today),
      validatePostalCode(userCountryPostalCodeVerifier)(form.postalCode)
    ).zipMap(User.apply)
  }

  private[homework2] def validatePostalCode(userCountryPostalCodeVerifier: String => Boolean)
                                           (postalCode: String)
  : Validated[RegistrationFormError, Option[String]] = {
    if (postalCode.isEmpty) {
      Valid(None)
    }
    else if (userCountryPostalCodeVerifier(postalCode)) {
      Valid(Some(postalCode))
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
    if (email.matches("[0-9a-zA-Z-_.]+@[0-9a-zA-Z-_.]+")) {
      val splitted = email.split("@")
      Valid(Email(splitted(0), splitted(1)))
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
        def hasAtLeastOneSpecialSymbol(password: String): Boolean = {
          val ordinary = (('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')).toSet

          password exists (!ordinary.contains(_))
        }

        def hasAtLeastOneDigit(password: String): Boolean = {
          val digits = ('0' to '9').toSet
          password exists (digits.contains(_))
        }

        def hasAtLeastOneLetter(password: String): Boolean = {
          val letters = (('a' to 'z') ++ ('A' to 'Z')).toSet

          password exists (letters.contains(_))
        }

        hasAtLeastOneLetter(password) && hasAtLeastOneDigit(password) && hasAtLeastOneSpecialSymbol(password)
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
    (
      validatePasswordsMatching(password, passwordConfirmation),
      validatePasswordLength(password),
      validatePasswordHasGoodSymbolVariety(password)
    ).zipMap((a, _, _) => a)
  }

  private[homework2] def validateBirthdayDate(birthYear: String, birthMonth: String, birthDay: String,
                                              today: Date) = {
    def isNonEmptyStringWithDigitsOnly(x: String) = (!x.isEmpty) && (x forall Character.isDigit)

    // ----------------------------------
    def validateBirthDay(birthDay: String) = {
      def validateDayIsAnInteger(birthDay: String) = {
        if (!isNonEmptyStringWithDigitsOnly(birthDay)) {
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
        if (!isNonEmptyStringWithDigitsOnly(birthMonth)) {
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
      if (!isNonEmptyStringWithDigitsOnly(birthYear)) {
        Invalid(YearIsNotAnInteger(birthYear))
      }
      else {
        Valid(birthYear.toInt)
      }

    // ----------------------------------
    def validateDate(birthYear: String, birthMonth: String, birthDay: String) = {
      (
        validateBirthYear(birthYear),
        validateBirthMonth(birthMonth),
        validateBirthDay(birthDay)
      ).zip match {
        case i@Invalid(_) => i
        case v@Valid(_) => {
          Date.applyOption(birthYear.toInt, birthMonth.toInt, birthDay.toInt) match {
            case None => Invalid(InvalidDate(Date(birthYear.toInt, birthMonth.toInt, birthDay.toInt)))
            case Some(date) => Valid(date)
          }
        }
      }
    }

    // ----------------------------------
    validateDate(birthYear, birthMonth, birthDay) match {
      case i@Invalid(_) => Invalid(InvalidBirthdayDate(i.errors.asInstanceOf[Chain[DateError]]))
      case Valid(date) => {
        if (date.isAfter(today)) {
          Invalid(BirthdayDateIsInTheFuture(date))
        }
        else {
          Valid(date)
        }
      }
    }
  }
}
