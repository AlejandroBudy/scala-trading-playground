//> using scala "3.6.3"
//> using dep org.typelevel::cats-core::2.13.0
//> using dep org.typelevel::cats-effect::3.6.0
//> using dep org.http4s::http4s-dsl::0.23.30
//> using dep org.http4s::http4s-ember-client::0.23.30
//> using dep org.http4s::http4s-circe::0.23.30
//> using dep io.circe::circe-generic::0.14.12
//> using dep io.circe::circe-literal::0.14.12
//
import cats.effect.unsafe.implicits.global
import cats.effect.IO
import io.circe.Json
import org.http4s.circe.*
import org.http4s.ember.client.EmberClientBuilder

val apikey = args.headOption.getOrElse(sys.error("No API key provided"))

EmberClientBuilder
    .default[IO]
    .build
    .use { client =>
        client.expect[Json](
          s"https://www.alphavantage.co/query?function=CURRENCY_EXCHANGE_RATE&from_currency=BTC&to_currency=EUR&apikey=$apikey"
        )
    }
    .flatMap(IO.println)
    .unsafeRunSync()
