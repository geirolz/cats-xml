package cats.xml.cursor

import cats.data.NonEmptyList
import cats.data.Validated.{Invalid, Valid}
import cats.xml.{Xml, XmlNode}
import cats.xml.cursor.NodeCursor.Root
import cats.xml.validator.Validator

class FreeCursorSuite extends munit.FunSuite {

  test("FreeCursor.focus - valid") {

    val cursor: FreeCursor[Xml, Int] = Root.bar.test.as[Int]
    val node: XmlNode =
      XmlNode("foo")
        .withChild(
          XmlNode("bar")
            .withChild(
              XmlNode("test").withText("1")
            )
        )

    assertEquals(
      obtained = cursor.focus(node),
      expected = Valid(1)
    )
  }

  test("FreeCursor.focus - invalid") {

    val cursor: FreeCursor[Xml, Int] = Root.bar.test.as[Int]
    val incompleteNode: XmlNode      = XmlNode("foo").withChild(XmlNode("bar"))

    assertEquals(
      obtained = cursor.focus(incompleteNode),
      expected = Invalid(NonEmptyList.of(CursorFailure.MissingNode("/bar", "test")))
    )
  }

  test("FreeCursor.map") {

    val cursor: FreeCursor[Xml, Int] = Root.bar.test.as[Int]
    val node: XmlNode =
      XmlNode("foo")
        .withChild(
          XmlNode("bar")
            .withChild(
              XmlNode("test").withText("1")
            )
        )

    assertEquals(
      obtained = cursor.map(_.toString).focus(node),
      expected = Valid("1")
    )
  }

  test("FreeCursor.validate - valid") {

    val cursor: FreeCursor[Xml, Int] = Root.bar.test.as[Int]
    val node: XmlNode =
      XmlNode("foo")
        .withChild(
          XmlNode("bar")
            .withChild(
              XmlNode("test").withText("1")
            )
        )

    assertEquals(
      obtained = cursor
        .validate(Validator.min(0).and(Validator.max(2)))
        .focus(node),
      expected = Valid(1)
    )
  }

  test("FreeCursor.validate - invalid") {

    val cursor: FreeCursor[Xml, Int] = Root.bar.test.as[Int]
    val node: XmlNode =
      XmlNode("foo")
        .withChild(
          XmlNode("bar")
            .withChild(
              XmlNode("test").withText("1")
            )
        )

    assertEquals(
      obtained = cursor
        .validate(Validator.range(10, 100).and(Validator.min(2)))
        .focus(node),
      expected = Invalid(
        NonEmptyList.of(
          CursorFailure.ValidationsFailed(
            path = "/bar/test",
            errors = NonEmptyList.of(
              "Value '1' is NOT in range [10 <= x <= 100]",
              "Value '1' is NOT >= '2'"
            )
          )
        )
      )
    )
  }
}

//class FreeCursorInstancesSuite extends munit.DisciplineSuite {
//
//  import cats.implicits.*
//  import cats.laws.discipline.arbitrary.*
//  import cats.xml.testing.codec.arbitrary.*
//
//  implicit def eqFreeCursor[I, O]: Eq[FreeCursor[I, O]] = Eq.allEqual
//
//  checkAll(
//    "FreeCursor.ApplicativeErrorTests",
//    ApplicativeErrorTests[FreeCursor[XmlNode, *], NonEmptyList[CursorFailure]]
//      .applicativeError[Int, Int, String]
//  )
//}
