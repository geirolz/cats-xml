package cats.xml.scalaxml.testing

import munit.{Assertions, Location}

import scala.xml.NodeSeq

trait NodeSeqAssertions { $this: munit.Suite =>

  import cats.xml.scalaxml.implicits.*

  def assertEqualsNodeSeq(
    obtained: NodeSeq,
    expected: NodeSeq,
    clue: => Any = "values are not the same"
  )(implicit loc: Location): Unit = {
    Assertions.assertEquals(
      obtained = obtained.normalized,
      expected = expected.normalized,
      clue     = clue
    )

    /* This to enforce the the `Eq` instance provided is equals to this assertion logic
       because `munit` at today doesn't support `assertEquals` using `Eq` instance.
     */
    Assertions.assert(
      obtained === expected
    )
  }
}
