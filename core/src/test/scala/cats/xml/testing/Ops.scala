package cats.xml.testing

import scala.concurrent.duration.{DurationLong, FiniteDuration}

object Ops {

  def timed[T](f: => T): (FiniteDuration, T) = {
    val start  = System.nanoTime()
    val result = f
    val end    = System.nanoTime()
    (end - start).nano -> result
  }
}
