package cats.xml.utils

import cats.Eq

trait UnderlyingThrowableWeakEq {

  val error: Throwable

  override def equals(obj: Any): Boolean =
    obj match {
      case keeper: UnderlyingThrowableWeakEq => Eq[UnderlyingThrowableWeakEq].eqv(this, keeper)
      case _                                 => false
    }
}
object UnderlyingThrowableWeakEq {
  implicit val weakEqUnderlyingThrowable: Eq[UnderlyingThrowableWeakEq] =
    (x: UnderlyingThrowableWeakEq, y: UnderlyingThrowableWeakEq) =>
      x.error == y.error || (
        x.error.getClass.isAssignableFrom(y.error.getClass) &&
          x.error.getCause == y.error.getCause &&
          x.error.getMessage == y.error.getMessage
      )
}
