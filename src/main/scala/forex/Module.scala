package forex

import cats.effect.{ ConcurrentEffect, Timer }
import dev.profunktor.redis4cats.RedisCommands
import forex.config.ApplicationConfig
import forex.http.rates.RatesHttpRoutes
import forex.http.health.HealthHttpRoutes
import forex.programs._
import forex.services._
import forex.services.quota.QuotaManager
import org.http4s._
import org.http4s.implicits._
import org.http4s.server.middleware.{ AutoSlash, Timeout }
import org.typelevel.log4cats.Logger
import cats.syntax.semigroupk._

class Module[F[_]: Timer: Logger: ConcurrentEffect](config: ApplicationConfig,
                                                    redis: RedisCommands[F, String, String]) {

  private val redisService = RatesServices.redisCall(redis, config)
  val quotaManager         = new QuotaManager[F](redis, config)

  private val cacheProgram: RatesProgram[F] = RatesProgram[F](redisService)

  private val ratesHttpRoutes: HttpRoutes[F]  = new RatesHttpRoutes[F](cacheProgram).routes
  private val healthHttpRoutes: HttpRoutes[F] = new HealthHttpRoutes[F](quotaManager, redisService).routes

  type PartialMiddleware = HttpRoutes[F] => HttpRoutes[F]
  type TotalMiddleware   = HttpApp[F] => HttpApp[F]

  private val routesMiddleware: PartialMiddleware = {
    { http: HttpRoutes[F] =>
      AutoSlash(http)
    }
  }

  private val appMiddleware: TotalMiddleware = { http: HttpApp[F] =>
    Timeout(config.http.timeout)(http)
  }

  private val http: HttpRoutes[F] = ratesHttpRoutes <+> healthHttpRoutes

  val oneFrameProgram     = RatesProgram[F](RatesServices.oneFrameCall[F](config))
  val httpApp: HttpApp[F] = appMiddleware(routesMiddleware(http).orNotFound)
  val redisRatesService   = redisService

}
