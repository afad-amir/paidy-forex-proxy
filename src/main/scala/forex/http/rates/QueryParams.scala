package forex.http.rates

import forex.domain.Currency
import forex.programs.rates.errors.{ toProgramError, Error }
import org.http4s.QueryParamDecoder
import org.http4s.dsl.impl.QueryParamDecoderMatcher
import cats.implicits._

object QueryParams {

  private[http] implicit val currencyQueryParam: QueryParamDecoder[Either[Error, Currency]] =
    QueryParamDecoder[String].map { cur =>
      Currency.fromString(cur).leftMap(toProgramError(_))
    }

  object FromQueryParam extends QueryParamDecoderMatcher[Error Either Currency]("from")
  object ToQueryParam extends QueryParamDecoderMatcher[Error Either Currency]("to")

}
