package forex.domain

import io.circe.{ Decoder, Encoder }
import io.circe.generic.semiauto._
import java.time.OffsetDateTime

object JsonCodecs {
  import Rate.Pair
  implicit val currencyEncoder: Encoder[Currency] = Encoder.encodeString.contramap[Currency](_.entryName)
  implicit val currencyDecoder: Decoder[Currency] = Decoder.decodeString.emap { str =>
    Currency.withNameOption(str).toRight(s"Invalid currency: $str")
  }

  implicit val offsetDateTimeEncoder: Encoder[OffsetDateTime] = Encoder.encodeOffsetDateTime
  implicit val offsetDateTimeDecoder: Decoder[OffsetDateTime] = Decoder.decodeOffsetDateTime

  implicit val timestampEncoder: Encoder[Timestamp] = Encoder.encodeOffsetDateTime.contramap[Timestamp](_.value)
  implicit val timestampDecoder: Decoder[Timestamp] = Decoder.decodeOffsetDateTime.map(Timestamp.apply)

  implicit val priceEncoder: Encoder[Price] = Encoder.encodeBigDecimal.contramap[Price](_.value)
  implicit val priceDecoder: Decoder[Price] = Decoder.decodeBigDecimal.map(Price.apply)

  implicit val pairEncoder: Encoder[Pair] = deriveEncoder[Pair]
  implicit val pairDecoder: Decoder[Pair] = deriveDecoder[Pair]

  implicit val rateEncoder: Encoder[Rate] = deriveEncoder[Rate]
  implicit val rateDecoder: Decoder[Rate] = deriveDecoder[Rate]
}
