package common

import cats.Applicative
import tofu.Raise

trait IntExecutor[F[_]] {
  def execute(i: Int): F[String]
}

object IntExecutor {

  abstract sealed class IntExecutorError(message: String) extends Throwable(message, null, true, false)

  final case object IntIsNegative extends IntExecutorError("Int should be 0 or positive")
  final case object IntTooBig     extends IntExecutorError("Int should be less than 100.")

  def apply[F[_]: Applicative](implicit R: Raise[F, IntExecutorError]): IntExecutor[F] = {
    case i: Int if i < 0   => R.raise(IntIsNegative)
    case i: Int if i > 100 => R.raise(IntTooBig)
    case i: Int            => Applicative[F].pure(s"Number is $i")
  }
}
