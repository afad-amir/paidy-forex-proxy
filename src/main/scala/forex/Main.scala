package forex

import cats.effect._
import dev.profunktor.redis4cats.Redis
import dev.profunktor.redis4cats.effect.Log.Stdout._
import forex.config._
import fs2.Stream
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

object Main extends IOApp {
  implicit def logger[F[_]: Sync]: Logger[F] = Slf4jLogger.getLogger[F]

  override def run(args: List[String]): IO[ExitCode] =
    application[IO].compile.drain.as(ExitCode.Success)

  def application[F[_]: ConcurrentEffect: Timer: ContextShift: Logger]: Stream[F, Unit] =
    for {
      config <- Config.stream[F]("app")
      redisUri = s"redis://${config.redis.host}:${config.redis.port}"
      _ <- Stream.eval(Logger[F].info("Starting Forex Application"))
      _ <- Stream.eval(Logger[F].info(s"HTTP server will bind to ${config.http.host}:${config.http.port}"))
      _ <- Stream.eval(
            Logger[F]
              .info(
                s"Main scheduler will update cache every ${(60 * 60 * 24.0 / config.oneFrame.quota.maxCallsPerDay).toInt} seconds"
              )
          )
      _ <- Stream.eval(
            Logger[F]
              .info(
                s"Quota refresh scheduler will check every ${config.oneFrame.quota.refreshCheckInterval.toSeconds} seconds"
              )
          )
      _ <- Stream
            .resource(Redis[F].utf8(redisUri))
            .flatMap { redis =>
              val module = new Module[F](config, redis)
              Stream(
                scheduler.scheduledUpdateWithBackoff(
                  config,
                  module.quotaManager,
                  module.oneFrameProgram,
                  module.redisRatesService
                ),
                scheduler.quotaRefreshScheduler(
                  config,
                  module.quotaManager
                ),
                org.http4s.blaze.server
                  .BlazeServerBuilder[F](scala.concurrent.ExecutionContext.global)
                  .bindHttp(config.http.port, config.http.host)
                  .withHttpApp(module.httpApp)
                  .serve
                  .drain
              ).parJoinUnbounded
            }
            .handleErrorWith { error =>
              Stream.eval(Logger[F].error(s"REDIS CONNECTION FAILED: ${error.getMessage}")) >>
                Stream.eval(Logger[F].info(s"RETRYING IN ${config.system.retryDelay.toSeconds} SECONDS...")) >>
                Stream.sleep[F](config.system.retryDelay) >>
                application[F]
            }
    } yield ()
}
