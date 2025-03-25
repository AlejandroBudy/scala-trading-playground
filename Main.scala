//> using scala "3.6.3"
//> using dep org.typelevel::cats-core::2.13.0
//> using dep org.typelevel::cats-effect::3.6.0
//> using dep org.http4s::http4s-dsl::0.23.30
//> using dep org.http4s::http4s-ember-client::0.23.30
//> using dep org.http4s::http4s-circe::0.23.30
//> using dep io.circe::circe-generic::0.14.12
//
import cats.effect.{IO, IOApp}
import cats.syntax.all.*
import io.circe.generic.auto.*
import io.circe.Decoder
import org.http4s.circe.jsonOf
import org.http4s.client.Client
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.Uri

object Main extends IOApp.Simple:
    given org.http4s.EntityDecoder[IO, StockResponse] = jsonOf[IO, StockResponse]

    val baseUri = Uri.unsafeFromString("https://www.alphavantage.co/query")

    case class TimeSeries(
        `1. open`: String,
        `2. high`: String,
        `3. low`: String,
        `4. close`: String,
        `5. volume`: String
    )

    case class StockResponse(`Time Series (Daily)`: Map[String, TimeSeries]) derives Decoder
    def buildClient: IO[Client[IO]] = EmberClientBuilder.default[IO].build.use(IO.pure)

    def fetchStock(client: Client[IO], symbol: String, apiKey: String): IO[Unit] =
        val uri = baseUri
            .withQueryParam("function", "TIME_SERIES_DAILY")
            .withQueryParam("symbol", symbol)
            .withQueryParam("apikey", apiKey)

        client
            .expect[StockResponse](uri)
            .flatMap { response =>
                val latestDate = response.`Time Series (Daily)`.keys.max
                val latestData = response.`Time Series (Daily)`(latestDate)
                IO.println(s"Últimos datos de $symbol ($latestDate):")
                    >> IO.println(s"Open: ${latestData.`1. open`}, Close: ${latestData.`4. close`}")
            }
            .handleErrorWith { error =>
                IO.println(s"Error al consultar $symbol: ${error.getMessage}")
            }
    end fetchStock

    def handleCommand(client: Client[IO], command: String, apiKey: String): IO[Boolean] =
        command.trim.toLowerCase match
        case "exit" =>
            IO.println("Saliendo...") *> IO.pure(false)
        case "etf" =>
            IO.println("Consultando ETF (ejemplo: SPY)") *> fetchStock(client, "SPY", apiKey) *> IO
                .pure(
                  true
                )
        case "stocks" =>
            IO.println("Consultando stock (ejemplo: AAPL)") *> fetchStock(
              client,
              "AAPL",
              apiKey
            ) *> IO
                .pure(true)
        case cmd if cmd.startsWith("stock ") =>
            val symbol = cmd.split(" ").last
            fetchStock(client, symbol, apiKey) *> IO.pure(true)
        case _ =>
            IO.println("Comando no reconocido. Opciones: etf, stocks, stock <symbol>, exit") *> IO
                .pure(true)

    def go(client: Client[IO])(apiKey: String): IO[Unit] = for
        _ <- IO.println(
          "Introduce un comando: ETF, stocks, stock <stock_name>, crypto <crypto_name>"
        )
        command  <- IO.readLine
        continue <- handleCommand(client, command, apiKey)
        _        <- if continue then go(client)(apiKey) else IO.unit
    yield ()

    def run: IO[Unit] =
        for
            _      <- IO.println("Bienvenido a la aplicación de consulta de stocks!")
            _      <- IO.println("Introduce tu api key de Alpha Vantage:")
            apiKey <- IO.readLine
            client <- buildClient
            _ <- if apiKey.trim.isEmpty then IO.println("Api key no válida") else go(client)(apiKey)
        yield ()

    end run

end Main
