package cats.xml.utils

object UnsafeValidator {

  @impure
  def unsafeRequire[T](value: T)(check: T => Boolean): T = {
    Predef.require(check(value))
    value
  }

  @impure
  def unsafeRequire[T](value: T, message: => Any)(check: T => Boolean): T = {
    Predef.require(check(value), message)
    value
  }

  @impure
  def unsafeRequireNotNull[T](value: T): T =
    unsafeRequire(value)(_ != null)
}
