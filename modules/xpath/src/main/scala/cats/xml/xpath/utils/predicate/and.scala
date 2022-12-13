package cats.xml.xpath.utils.predicate

import cats.Monoid

object and {
  implicit def logicalAndMonoid[T]: Monoid[T => Boolean] =
    new Monoid[T => Boolean] {
      override def empty: T => Boolean = _ => true

      override def combine(x: T => Boolean, y: T => Boolean): T => Boolean =
        t => x(t) && y(t)
    }
}
