package common

import cats.Applicative
import tofu.Raise
import tofu.syntax.raise._

import scala.util.Try

trait  IntParser[F[_]] {
  def parse(s: String): F[Int]
}

object IntParser {

  abstract sealed class IntParserError(message: String) extends Throwable(message, null, true, false)

  final case class NotInt(v: String) extends IntParserError(s"$v cannot be parsed as Int")

  def apply[F[_]: Applicative](implicit R: Raise[F, IntParserError]): IntParser[F] = (s: String) => Try(s.toInt).toOption.orRaise(NotInt(s))
}
