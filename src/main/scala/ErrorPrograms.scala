import cats.Monad
import common.IntExecutor.{IntExecutorError, IntTooBig}
import common.IntParser.IntParserError
import common.{IntExecutor, IntParser}
import tofu.common.Console
import tofu.syntax.console._
import tofu.syntax.handle._
import tofu.syntax.monadic._
import tofu.{Handle, Raise}

object ErrorPrograms {

  //Handles all errors
  def general[F[_]: Monad: Console: Handle[*[_], IntExecutorError]: Handle[*[_], IntParserError]](
    input: String,
    parser: IntParser[F],
    executor: IntExecutor[F]
  ): F[String] = {
    val flow = for {
      parsed <- parser.parse(input)
      result <- executor.execute(parsed)
    } yield result

    flow
      .handleWith[IntParserError](IntParserError.handleIntParseError(_))
      .handleWith[IntExecutorError](IntExecutorError.handleIntExecutorError(_))
  }

  //handles only specific error
  def specific[F[_]: Monad: Console: Handle[*[_], IntTooBig.type]](
    input: String,
    parser: IntParser[F],
    executor: IntExecutor[F]
  ): F[String] = {
    val flow = for {
      parsed <- parser.parse(input)
      result <- executor.execute(parsed)
    } yield result

    flow.handle[IntTooBig.type](e => s"Tooooooo big for me: ${e.getMessage}")
  }
}
