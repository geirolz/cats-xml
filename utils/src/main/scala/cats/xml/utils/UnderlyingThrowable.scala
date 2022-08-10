package cats.xml.utils

import cats.Eq

trait UnderlyingThrowable {

  val error: Throwable

  override def equals(obj: Any): Boolean =
    obj match {
      case keeper: UnderlyingThrowable => Eq[UnderlyingThrowable].eqv(this, keeper)
      case _                           => false
    }
}
object UnderlyingThrowable {
  implicit val weakEqUnderlyingThrowable: Eq[UnderlyingThrowable] =
    (x: UnderlyingThrowable, y: UnderlyingThrowable) =>
      x.error == y.error || (
        x.error.getClass.isAssignableFrom(y.error.getClass) &&
          x.error.getCause == y.error.getCause &&
          x.error.getMessage == y.error.getMessage
      )
}
