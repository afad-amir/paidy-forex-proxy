package forex.services.rates.interpreters

import cats.Applicative
import cats.data.NonEmptyList
import cats.effect.concurrent.Ref
import cats.implicits._
import forex.domain.Rate
import forex.http.rates.Protocol.ErrorResponse
import forex.services.rates.Algebra
import forex.services.rates.errors.Error
class CachedOneFrame[F[_]: Applicative](mapRef: Ref[F, Map[Rate.Pair, Rate]]) extends Algebra[F] {
  override def get(pair: Rate.Pair): F[Error Either Rate] =
    getAll(NonEmptyList.one(pair)).map {
      case Right(rates) => rates.head.asRight
      case Left(err)    => err.asLeft
    }

  override def getAll(pairs: NonEmptyList[Rate.Pair]): F[Error Either NonEmptyList[Rate]] =
    if (pairs.forall(p => p.from != p.to)) lookup(pairs)
    else ErrorResponse("Double Pair").toDomain.asLeft[NonEmptyList[Rate]].pure[F]

  private def lookup(pairs: NonEmptyList[Rate.Pair]): F[Error Either NonEmptyList[Rate]] = {
    val allPairsF = mapRef.get.map(pair => pairs.toList.flatMap(p => pair.get(p)))
    val ratesOptF = allPairsF.map(p => NonEmptyList.fromList(p))
    for {
      ratesOpt <- ratesOptF
      ethResponse = Either.fromOption(
        ratesOpt,
        Error.RateLookupFailed("Rate not Available")
      )
    } yield ethResponse
  }
}
