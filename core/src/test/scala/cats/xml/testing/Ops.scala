package cats.xml.testing

import scala.concurrent.duration.{DurationLong, FiniteDuration}

object Ops {

  def timed[T](f: => T): (FiniteDuration, T) = {
    val start  = System.nanoTime()
    val result = f
    val end    = System.nanoTime()
    (end - start).nano -> result
  }

  // returns a 32-character MD5 hash version of the input string
  def md5(data: String): String = {
    import java.math.BigInteger
    import java.security.MessageDigest
    val md                  = MessageDigest.getInstance("MD5")
    val digest: Array[Byte] = md.digest(data.getBytes)
    val bigInt              = new BigInteger(1, digest)
    val hashedPassword      = bigInt.toString(16).trim
    prependWithZeros(hashedPassword)
  }

  /** This uses a little magic in that the string I start with is a “format specifier,” and it
    * states that the string it returns should be prepended with blank spaces as needed to make the
    * string length equal to 32. Then I replace those blank spaces with the character `0`.
    */
  private def prependWithZeros(pwd: String): String =
    "%1$32s".format(pwd).replace(' ', '0')
}
