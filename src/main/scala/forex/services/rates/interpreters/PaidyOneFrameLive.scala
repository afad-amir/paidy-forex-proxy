package forex.services.rates.interpreters

import cats.data.NonEmptyList
import cats.effect.ConcurrentEffect
import cats.implicits.{
  catsSyntaxApplicativeError,
  catsSyntaxApplicativeId,
  catsSyntaxEitherId,
  catsSyntaxFlatMapOps,
  toFlatMapOps,
  toFunctorOps,
  toTraverseOps
}
import forex.config.ApplicationConfig
import forex.domain.Rate
import forex.http.rates.Protocol.{ ErrorResponse, RateResponse }
import forex.services.rates.Algebra
import forex.services.rates.errors.Error
import org.http4s._
import org.http4s.blaze.client.BlazeClientBuilder
import org.http4s.client.dsl.Http4sClientDsl
import org.http4s.headers.Accept
import org.typelevel.ci.CIString
import org.typelevel.log4cats.Logger

import scala.concurrent.ExecutionContext

class PaidyOneFrameLive[F[_]: Logger: ConcurrentEffect](config: ApplicationConfig)
    extends Algebra[F]
    with Http4sClientDsl[F] {
  import org.http4s.circe._
  override def get(pair: Rate.Pair): F[Error Either Rate] =
    getAll(NonEmptyList.one(pair)).map(_.map(_.head))

  override def getAll(pairs: NonEmptyList[Rate.Pair]): F[Either[Error, NonEmptyList[Rate]]] = getRaw(pairs).map {
    case Left(error)             => error.toDomain.asLeft
    case Right(rateResponseList) => rateResponseList.map(_.toDomain).sequence
  }

  private def createRequest(pairs: NonEmptyList[Rate.Pair]): Request[F] = {
    val pairStrings = pairs.map { pair =>
      s"${pair.from}${pair.to}"
    }.toList
    val query = Query.fromMap(Map("pair" -> pairStrings))

    val uri        = Uri.unsafeFromString(config.oneFrame.uri)
    val updatedUri = (uri / "rates").copy(query = query)
    Request[F](
      uri = updatedUri,
      headers = Headers(
        Accept(MediaType.application.json),
        Header.Raw(CIString("token"), config.oneFrame.token)
      )
    )

  }

  private def getRaw(
      pairs: NonEmptyList[Rate.Pair]
  ): F[ErrorResponse Either NonEmptyList[RateResponse]] = {
    val request = createRequest(pairs)

    def getRates(response: Response[F]): F[ErrorResponse Either NonEmptyList[RateResponse]] =
      if (response.status == Status.Ok)
        response.asJsonDecode[List[RateResponse]].attempt.flatMap {
          case Left(_)    => response.asJsonDecode[ErrorResponse].map(_.asLeft)
          case Right(Nil) => ErrorResponse("Double Rate.Pair").asLeft[NonEmptyList[RateResponse]].pure[F]
          case Right(rates) =>
            NonEmptyList.fromListUnsafe(rates).asRight.pure[F]
        } else
        Logger[F].error(s"Client Call Failed.\nRequest:$request\nResponse:$response") >> ErrorResponse(
          "Client Call Failed"
        ).asLeft.pure[F]

    BlazeClientBuilder[F](ExecutionContext.global).resource
      .evalMap { client =>
        client.run(request).use { response =>
          getRates(response)
        }
      }
      .use(_.pure[F])

  }
}
