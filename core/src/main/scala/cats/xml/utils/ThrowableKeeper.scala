package cats.xml.utils

trait ThrowableKeeper {

  val ex: Throwable

  override def equals(obj: Any): Boolean =
    if (obj.isInstanceOf[ThrowableKeeper]) {
      val thatEx = asInstanceOf[ThrowableKeeper].ex
      thatEx.getClass.isAssignableFrom(ex.getClass) && thatEx.getMessage == ex.getMessage
    } else {
      false
    }
}
