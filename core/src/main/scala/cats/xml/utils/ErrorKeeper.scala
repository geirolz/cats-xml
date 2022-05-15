package cats.xml.utils

import cats.Eq

trait ErrorKeeper {

  val error: Throwable

  override def equals(obj: Any): Boolean =
    obj match {
      case keeper: ErrorKeeper => Eq[ErrorKeeper].eqv(this, keeper)
      case _                   => false
    }
}
object ErrorKeeper {
  implicit val eqErrorKeeper: Eq[ErrorKeeper] =
    (x: ErrorKeeper, y: ErrorKeeper) =>
      x.error == y.error ||
        x.error.getClass.isAssignableFrom(y.error.getClass) &&
        x.error.getCause == y.error.getCause &&
        x.error.getMessage == y.error.getMessage
}
