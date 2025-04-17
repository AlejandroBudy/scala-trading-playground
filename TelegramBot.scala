//> using scala "3.6.3"
//> using dep org.typelevel::cats-core::2.13.0
//> using dep org.typelevel::cats-effect::3.6.0
//> using dep org.augustjune::canoe::0.6.0

import canoe.api.*
import canoe.models.outgoing.{MessageContent, TextContent}
import canoe.models.Chat
import canoe.syntax.*
import cats.effect.{IO, IOApp}
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
        IO.envForIO
            .get("TELEGRAM_BOT_TOKEN")
            .flatMap(IO.fromOption(_)(orElse = new RuntimeException("No token found")))
            .flatMap { token =>
                Stream
                    .resource(TelegramClient[IO](token))
                    .flatMap { client =>
                        given TelegramClient[IO] = client
                        Bot.polling[IO].follow(handleCommand)
                    }
                    .compile
                    .drain
            }

    def greetings[F[_]: TelegramClient]: Scenario[F, Unit] =
        for
            chat <- Scenario.expect(command("start").chat)
            _    <- Scenario.expect(text)
            _    <- Scenario.eval(chat.send("Hello! what's your name?"))
            _ = println(chat)
            name <- Scenario.expect(text)
            f    <- Scenario.eval(chat.send(s"Nice to meet you, $name!"))
        yield ()

    def handleCommand(using TelegramClient[IO]): Scenario[IO, Unit] =
        for
            chat <- Scenario.expect(command("start").chat)
            _    <- Scenario.eval(chat.send("Bienvenido! prueba con el comando /help"))
            msg  <- Scenario.expect(text)
            _ = println(chat)
            _ <-
                if msg.startsWith("/") then go(msg.trim.substring(1))(chat)
                else Scenario.eval(chat.send("Unknown command. Please try again."))
        yield ()

    def go(str: String)(chat: Chat)(using TelegramClient[IO]): Scenario[IO, Unit] =
        Scenario
            .eval(chat.send(s"tu comando $str"))
            .unit

    extension [A](a: Scenario[IO, A])
        def unit: Scenario[IO, Unit] =
            a.map(_ => ())
end TelegramBot
