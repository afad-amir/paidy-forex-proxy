package forex.services.rates

object errors {

  sealed trait Error
  object Error {
    final case class CurrencyNotSupported(requestedCurr: Option[String] = None) extends Error
    final case object DoublePair extends Error
    final case class RateLookupFailed(msg: String) extends Error
  }

}
