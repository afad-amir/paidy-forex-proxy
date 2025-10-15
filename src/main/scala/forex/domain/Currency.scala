package forex.domain

import cats.Show
import cats.data.NonEmptyList
import cats.implicits.toBifunctorOps
import enumeratum._
import forex.services.rates.errors.Error.CurrencyNotSupported
import forex.services.rates.errors._

sealed trait Currency extends EnumEntry
object Currency extends Enum[Currency] with CatsEnum[Currency] {

  val values = findValues
  def makePairs[A](set: Set[A]): Set[(A, A)] =
    for {
      n <- set
      n1 <- set - n
    } yield (n, n1)

  case object AUD extends Currency
  case object CAD extends Currency
  case object CHF extends Currency
  case object EUR extends Currency
  case object GBP extends Currency
  case object NZD extends Currency
  case object JPY extends Currency
  case object SGD extends Currency
  case object USD extends Currency

  implicit val show: Show[Currency] = Show.show {
    case AUD => "AUD"
    case CAD => "CAD"
    case CHF => "CHF"
    case EUR => "EUR"
    case GBP => "GBP"
    case NZD => "NZD"
    case JPY => "JPY"
    case SGD => "SGD"
    case USD => "USD"
  }

  def fromString(s: String): Error Either Currency =
    Currency.withNameInsensitiveEither(s).leftMap {
      case _ => CurrencyNotSupported(Some(s))
    }

  lazy val allCombinations: NonEmptyList[(Currency, Currency)] = {
    val pairs = makePairs(values.toSet).toList
    NonEmptyList.fromListUnsafe(pairs)
  }
  lazy val allCombinationsLength = allCombinations.length
}
