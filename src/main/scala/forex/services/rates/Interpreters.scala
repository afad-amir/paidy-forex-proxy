package forex.services.rates

import cats.Applicative
import cats.effect.{ Clock, ConcurrentEffect, Timer }
import cats.effect.concurrent.Ref
import dev.profunktor.redis4cats.RedisCommands
import forex.config.ApplicationConfig
import forex.domain.Rate
import interpreters._
import org.typelevel.log4cats.Logger

object Interpreters {
  def oneFrameCall[F[_]: ConcurrentEffect: Logger](config: ApplicationConfig): Algebra[F] =
    new PaidyOneFrameLive[F](config)

  def inMemoryCall[F[_]: Applicative](cacheRef: Ref[F, Map[Rate.Pair, Rate]]): Algebra[F] =
    new CachedOneFrame[F](cacheRef)

  def redisCall[F[_]: ConcurrentEffect: Logger: Clock: Timer](
      redis: RedisCommands[F, String, String],
      config: ApplicationConfig
  ): RedisRatesService[F] =
    new RedisRatesService[F](redis, config)
}
