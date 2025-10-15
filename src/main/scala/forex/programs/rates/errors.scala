package forex.programs.rates

import forex.services.rates.errors.{ Error => RatesServiceError }

object errors {

  sealed trait Error extends Exception
  object Error {
    final case class RateLookupFailed(msg: String) extends Error
    final case object DoublePair extends Error
    final case class CurrencyNotSupported(requestedCurr: Option[String] = None) extends Error
  }

  def toProgramError(error: RatesServiceError): Error = error match {
    case RatesServiceError.RateLookupFailed(msg)      => Error.RateLookupFailed(msg)
    case RatesServiceError.CurrencyNotSupported(curr) => Error.CurrencyNotSupported(curr)
    case RatesServiceError.DoublePair                 => Error.DoublePair
  }
}
