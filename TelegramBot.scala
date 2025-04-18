//> using scala "3.6.3"
//> using dep org.typelevel::cats-core::2.13.0
//> using dep org.typelevel::cats-effect::3.6.0
//> using dep org.augustjune::canoe::0.6.0

import canoe.api.*
import canoe.models.outgoing.{MessageContent, TextContent}
import canoe.models.Chat
import canoe.syntax.*
import cats.effect.{IO, IOApp, Ref, Temporal}
import cats.syntax.all.*
import fs2.Stream

enum Command:
    case Start
    case Help

final case class State(
    command: Command
)
object TelegramBot extends IOApp.Simple:

    def run: IO[Unit] =
        for
            _   <- IO.println("Starting Telegram bot...")
            _   <- IO.println("Make sure to set the TELEGRAM_BOT_TOKEN environment variable.")
            ref <- Ref.of[IO, Map[String, String]](Map.empty)
            maybeToken <- IO.envForIO.get("TELEGRAM_BOT_TOKEN")
            token      <- IO.fromOption(maybeToken)(orElse = new RuntimeException("No token found"))
            _ <- Stream
                .resource(TelegramClient[IO](token))
                .flatMap { client =>
                    given TelegramClient[IO] = client
                    Bot.polling[IO].follow(handleCommand(ref))
                }
                .compile
                .drain
        yield ()
    end run

    def handleCommand(
        db: Ref[IO, Map[String, String]]
    )(using TelegramClient[IO]): Scenario[IO, Unit] =
        for
            chat <- Scenario.expect(command("start").chat)
            _    <- Scenario.eval(chat.send("Bienvenido! prueba con el comando /help"))
            _    <- go(chat, db)
        yield ()

    def go(chat: Chat, db: Ref[IO, Map[String, String]])(using
        TelegramClient[IO]
    ): Scenario[IO, Unit] =
        for
            msg <- Scenario.expect(text)
            _ <-
                if !msg.startsWith("/") then
                    Scenario.eval(chat.send("Unknown command. Please try again."))
                else
                    msg.trim.toLowerCase match
                    case "/help" =>
                        Scenario
                            .eval(chat.send("""
                                                  |/etf <symbol>: Get ETF information
                                                  |/stock <symbol>: Get stock information
                                                  |/crypto <symbol>: Get crypto information
                                                  |/settoken: Set the API token
                                                  |""".stripMargin)) >> go(chat, db)
                    case "/settoken" =>
                        for
                            _     <- Scenario.eval(chat.send("Please enter the API token:"))
                            token <- Scenario.expect(text)
                            _     <- Scenario.eval(db.update(_ + (chat.id.toString -> token)))
                            _     <- Scenario.eval(chat.send("Token set"))
                            _ <- Scenario.eval(
                              chat.send("You can now use the token for API requests.")
                            )
                            _ <- go(chat, db)
                        yield ()
        yield ()

    extension [A](a: Scenario[IO, A])
        def unit: Scenario[IO, Unit] =
            a.map(_ => ())
end TelegramBot
