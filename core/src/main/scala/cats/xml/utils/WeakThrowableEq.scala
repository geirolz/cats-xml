package cats.xml.utils

trait WeakThrowableEq[T <: WeakThrowableEq[T]] {

  val ex: Throwable

  override def equals(obj: Any): Boolean =
    if (obj.isInstanceOf[WeakThrowableEq[?]]) {
      val thatEx = asInstanceOf[WeakThrowableEq[?]].ex
      thatEx.getClass.isAssignableFrom(ex.getClass) && thatEx.getMessage == ex.getMessage
    } else {
      false
    }
}
