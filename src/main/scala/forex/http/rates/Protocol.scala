package forex.http
package rates

import forex.domain.Currency.show
import forex.domain._
import forex.services.rates.errors.Error
import io.circe._
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto.deriveConfiguredEncoder
import io.circe.generic.semiauto.deriveDecoder

import java.time.OffsetDateTime

object Protocol {

  import Rate.Pair
  implicit val configuration: Configuration = Configuration.default.withSnakeCaseMemberNames

  final case class GetApiRequest(
      from: Currency,
      to: Currency
  )

  final case class GetApiResponse(
      from: Currency,
      to: Currency,
      price: Price,
      last_refreshed: Timestamp,
      bid: BigDecimal,
      ask: BigDecimal
  )
  final case class RateResponse(
      from: Currency,
      to: Currency,
      bid: BigDecimal,
      ask: BigDecimal,
      price: BigDecimal,
      time_stamp: OffsetDateTime
  ) {
    def toDomain: Error Either Rate = Rate.create(from, to, price, bid, ask, time_stamp)
  }
  final case class ErrorResponse(error: String) {
    def toDomain: Error =
      error match {
        case "Invalid Currency Pair" => Error.CurrencyNotSupported()
        case "Double Pair"           => Error.DoublePair
        case msg                     => Error.RateLookupFailed(msg)
      }
  }
  implicit val currencyEncoder: Encoder[Currency] =
    Encoder.instance[Currency] { show.show _ andThen Json.fromString }

  implicit val pairEncoder: Encoder[Pair] =
    deriveConfiguredEncoder[Pair]

  implicit val rateEncoder: Encoder[Rate] =
    deriveConfiguredEncoder[Rate]

  implicit val responseEncoder: Encoder[GetApiResponse] =
    deriveConfiguredEncoder[GetApiResponse]
  implicit val errResponseDec: Decoder[ErrorResponse] = deriveDecoder[ErrorResponse]
  implicit val rateResponseDec: Decoder[RateResponse] = deriveDecoder[RateResponse]

}
