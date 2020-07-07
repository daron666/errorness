import cats.Monad
import common.IntExecutor.{IntExecutorError, IntTooBig}
import common.IntParser.IntParserError
import common.{IntExecutor, IntParser}
import tofu.Handle
import tofu.common.Console
import tofu.syntax.handle._
import tofu.syntax.monadic._

object ErrorPrograms {

  //Handles all errors
  //Here we can see both handles in signature, that indicates that this method is able tp handle them.
  def general[F[_]: Monad: Console: Handle[*[_], IntExecutorError]: Handle[*[_], IntParserError]](
    input: String,
    parser: IntParser[F],
    executor: IntExecutor[F]
  ): F[String] = {
    val flow = for {
      parsed <- parser.parse(input)
      result <- executor.execute(parsed)
    } yield result

    //Here is the chain of handling. We can handle each ADT with it's root type
    flow
      .handleWith[IntParserError](IntParserError.handleIntParseError(_))
      .handleWith[IntExecutorError](IntExecutorError.handleIntExecutorError(_))
  }

  //Handles only specific error
  //Here signature indicates that this method gonna handle only one specific type.
  def specific[F[_]: Monad: Console: Handle[*[_], IntTooBig.type]](
    input: String,
    parser: IntParser[F],
    executor: IntExecutor[F]
  ): F[String] = {
    val flow = for {
      parsed <- parser.parse(input)
      result <- executor.execute(parsed)
    } yield result

    //Here is example how to handle only specific error.
    flow.handle[IntTooBig.type](e => s"Tooooooo big for me: ${e.getMessage}")
  }
}
