package forex.services.rates

import cats.data.NonEmptyList
import forex.domain.Rate
import forex.services.rates.errors._

trait Algebra[F[_]] {
  def get(pair: Rate.Pair): F[Error Either Rate]

  def getAll(pairs: NonEmptyList[Rate.Pair]): F[Error Either NonEmptyList[Rate]]

  def getAllRates: fs2.Stream[F, Rate] =
    fs2.Stream
      .eval(getAll(Rate.Pair.allCurrencyPairs))
      .filter(_.isRight)
      .map {
        case Right(value) => value.toList
        case Left(_)      => Nil
      }
      .flatMap(fs2.Stream.apply)

}
