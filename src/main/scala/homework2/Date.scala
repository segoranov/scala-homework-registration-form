package homework2

import java.time.{LocalDate}

import scala.util.Try

case class Date(year: Int, month: Int, day: Int){
  def isInTheFutre = LocalDate.of(year, month, day).isAfter(LocalDate.now())
}

object Date {
  def applyOption(year: Int, month: Int, day: Int): Option[Date] = Try {
    LocalDate.of(year, month, day)

    Date(year, month, day)
  }.toOption
}
