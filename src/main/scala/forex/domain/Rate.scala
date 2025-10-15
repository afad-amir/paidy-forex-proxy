package forex.domain

import cats.Show
import cats.data.NonEmptyList
import cats.implicits.catsSyntaxEitherId
import forex.services.rates.errors.Error
import java.time.OffsetDateTime

case class Rate(
    pair: Rate.Pair,
    price: Price,
    bid: BigDecimal,
    ask: BigDecimal,
    timestamp: Timestamp
)

object Rate {
  final case class Pair(
      from: Currency,
      to: Currency
  )
  object Pair {
    def create(from: Currency, to: Currency): Either[Error, Rate.Pair] =
      if (from == to) Error.DoublePair.asLeft
      else Pair(from, to).asRight

    implicit val pairShow: Show[Pair] = Show.show[Pair](p => s"${p.from}${p.to}")

    def allCurrencyPairs: NonEmptyList[Pair] = Currency.allCombinations.map { case (c1, c2) => Pair(c1, c2) }

  }
  def create(
      from: Currency,
      to: Currency,
      price: BigDecimal,
      bid: BigDecimal,
      ask: BigDecimal,
      timeStamp: OffsetDateTime
  ): Error Either Rate =
    Pair.create(from, to).map { pair =>
      Rate(pair, Price(price), bid, ask, Timestamp(timeStamp))
    }

}
