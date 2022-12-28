package cats.xml.testing.arbitrary

import org.scalacheck.Arbitrary

object CommonArbitrary {

  implicit def arbSome[T: Arbitrary]: Arbitrary[Some[T]] = Arbitrary {
    implicitly[Arbitrary[T]].arbitrary.map(Some(_))
  }
}
