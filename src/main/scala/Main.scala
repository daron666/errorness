import cats.{Applicative, Monad}
import cats.effect.{ExitCode, IO, IOApp}
import common.{IntExecutor, IntParser}
import tofu.Handle
import tofu.common.Console
import tofu.syntax.console._
import tofu.syntax.handle._
import tofu.syntax.monadic._

object Main extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    val parser   = IntParser[IO]
    val executor = IntExecutor[IO]

    for {
      _ <- runCase(ErrorPrograms.general[IO]("42", parser, executor), "General Case. Happy path.").attempt
      _ <- runCase(ErrorPrograms.general[IO]("4two", parser, executor), "General Case. Parse error.").attempt
      _ <- runCase(ErrorPrograms.general[IO]("152", parser, executor), "General Case. Int not suitable.").attempt
      _ <- runCase(ErrorPrograms.specific[IO]("-1", parser, executor), "Specific Case. Int is negative. Won't handling.").attempt
      _ <- runCase(ErrorPrograms.specific[IO]("151", parser, executor), "Specific Case. Int is too big. Gonna handle.").attempt
      _ <- runCase(ErrorPrograms.specific[IO]("43", parser, executor), "Specific Case. Happy path.").attempt
    } yield ExitCode.Success

  }

  def runCase[F[_]: Console: Monad: Handle[*[_], Throwable]](f: F[String], name: String): F[Unit] =
    for {
      _      <- putStrLn(s"Starting case $name")
      result <- f.handleWith[Throwable](defaultHandle(_))
      _      <- putStrLn(s"Result is:    $result")
      _      <- putStrLn("")
    } yield ()

  def defaultHandle[F[_]: Applicative](e: Throwable): F[String] = s"UncaughtError: ${e.getMessage}".pure

}
