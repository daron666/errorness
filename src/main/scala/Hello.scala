import cats.effect.{ExitCode, IO, IOApp}
import common.IntExecutor.{IntExecutorError, IntIsNegative, IntTooBig}
import common.IntParser.{IntParserError, NotInt}
import common.{IntExecutor, IntParser}
import tofu.{Errors, ErrorsTo, Handle}
import tofu.syntax.handle._

object Hello extends IOApp {

  val parser   = IntParser[IO]
  val executor = IntExecutor[IO]

  def program(input: String): IO[ExitCode] =
    for {
      parsed   <- parser.parse(input)
      executed <- executor.execute(parsed)
      _        <- IO.delay(println(executed))
    } yield ExitCode.Success

  override def run(args: List[String]): IO[ExitCode] = {
    val input = if (args.nonEmpty) { args.head }
    else { "42" }
    program(input).handleWith(intExecErrors).handleWith(parseErrors)
  }

  private def intExecErrors(e: IntExecutorError): IO[ExitCode] = e match {
    case _: IntTooBig.type =>
      for {
        _ <- IO.delay(println("Got too big value for int."))
        r <- IO.pure(ExitCode.Success)
      } yield r
    case _: IntIsNegative.type =>
      for {
        _ <- IO.delay(println("Got negative value."))
        r <- IO.pure(ExitCode.Success)
      } yield r
  }

  private def parseErrors(e: IntParserError): IO[ExitCode] = e match {
    case NotInt(v) =>
      for {
        _ <- IO.delay(println(s"Cannot convert $v to Int."))
        r <- IO.pure(ExitCode.Success)
      } yield r
  }

}
