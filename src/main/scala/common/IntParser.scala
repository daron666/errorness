package common

import cats.Applicative
import tofu.Raise
import tofu.syntax.monadic._
import tofu.syntax.raise._

import scala.util.Try

trait IntParser[F[_]] {
  def parse(s: String): F[Int]
}

object IntParser {

  abstract sealed class IntParserError(message: String) extends Throwable(message, null, true, false)

  object IntParserError {

    def handleIntParseError[F[_]: Applicative](e: IntParserError): F[String] = e match {
      case NotInt(v) => s"Cannot parse $v as int.".pure
      case _         => s"This case won't happen anyhow".pure
    }
  }

  final case class NotInt(v: String) extends IntParserError(s"$v cannot be parsed as Int")

  def apply[F[_]: Applicative](implicit R: Raise[F, IntParserError]): IntParser[F] = (s: String) => Try(s.toInt).toOption.orRaise(NotInt(s))
}
