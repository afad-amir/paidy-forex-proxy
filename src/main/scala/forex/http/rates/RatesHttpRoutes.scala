package forex.http
package rates

import cats.effect.Sync
import cats.implicits.catsSyntaxApplicativeError
import cats.syntax.flatMap._
import forex.programs.RatesProgram
import forex.programs.rates.{ Protocol => RatesProgramProtocol }
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import forex.programs.rates.errors.Error
import org.http4s.circe.CirceEntityCodec.circeEntityEncoder
import org.http4s.server.Router
import org.typelevel.log4cats.Logger
class RatesHttpRoutes[F[_]: Sync: Logger](rates: RatesProgram[F]) extends Http4sDsl[F] {

  import Converters._
  import Protocol._
  import QueryParams._

  private val httpRoutes: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root :? FromQueryParam(fromEth) +& ToQueryParam(toEth) =>
      Sync[F]
        .fromEither(fromEth)
        .flatMap { from =>
          Sync[F].fromEither(toEth).flatMap { to =>
            Logger[F]
              .info(s"SERVICE CALL: Requesting rate for pair ${from.toString}/${to.toString} from RatesProgram") >>
              rates.get(RatesProgramProtocol.GetRatesRequest(from, to)).flatMap(Sync[F].fromEither).flatMap { rate =>
                Ok(rate.asGetApiResponse)
              }
          }
        }
        .handleErrorWith {
          case Error.CurrencyNotSupported(Some(curr)) =>
            Logger[F].warn(s"API ERROR: Currency ${curr.toUpperCase} is not supported") >>
              BadRequest(s"Currency ${curr.toUpperCase} is not supported.")
          case Error.DoublePair =>
            BadRequest(s"Pair must have different currencies")
          case Error.RateLookupFailed(msg) =>
            Logger[F].warn(s"API WARNING: Rate lookup failed -> $msg") >>
              ServiceUnavailable(s"Rate not available: $msg. This may be temporary if the external service is down.")
          case err =>
            Logger[F].error(s"API ERROR: Unexpected error -> ${err.getMessage}") >>
              InternalServerError()
        }
    case GET -> Root =>
      Logger[F].info(s"SERVICE CALL: Requesting all rates from RatesProgram") >>
        rates.allRates.compile.toList
          .flatMap { ratesList =>
            if (ratesList.nonEmpty) {
              Logger[F].info(s"API BULK SUCCESS: Returning ${ratesList.length} rates to client") >>
                Ok(ratesList.map(_.asGetApiResponse))
            } else {
              Logger[F].warn(s"API BULK WARNING: No rates available in cache - external service may be down") >>
                ServiceUnavailable(
                  "Rates are currently unavailable. Please try again later when the external service is accessible."
                )
            }
          }
          .handleErrorWith { error =>
            Logger[F].error(s"API BULK ERROR: Failed to retrieve all rates -> ${error.getMessage}") >>
              InternalServerError("Unable to retrieve rates at this time. Please try again later.")
          }
  }

  val routes: HttpRoutes[F] = Router("/rates" -> httpRoutes)

}
