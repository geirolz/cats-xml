package cats.xml.validator

import cats.data.NonEmptyList
import cats.data.Validated.{Invalid, Valid}
import org.scalacheck.Prop.forAll

class ValidatorSuite extends munit.ScalaCheckSuite {

  test("Validator.of") {

    val validator: Validator[Int] = Validator.of {
      case 1 => Valid(1)
      case _ => Invalid(NonEmptyList.one("ERROR"))
    }

    assertEquals(
      obtained = validator(1),
      expected = Valid(1)
    )

    assertEquals(
      obtained = validator(2),
      expected = Invalid(NonEmptyList.one("ERROR"))
    )
  }

  test("Validator.mustBe") {

    val validator: Validator[Int] = Validator.mustBe[Int](t => s"Value $t failed!") {
      case 1 => true
      case _ => false
    }

    assertEquals(
      obtained = validator(1),
      expected = Valid(1)
    )

    assertEquals(
      obtained = validator(2),
      expected = Invalid(NonEmptyList.one("Value 2 failed!"))
    )
  }

  test("Validator.mustNotBe") {

    val validator: Validator[Int] = Validator.mustNotBe[Int](t => s"Value $t failed!") {
      case 1 => false
      case _ => true
    }

    assertEquals(
      obtained = validator(1),
      expected = Valid(1)
    )

    assertEquals(
      obtained = validator(2),
      expected = Invalid(NonEmptyList.one("Value 2 failed!"))
    )
  }

  test(s"Validator.and") {

    val lessThan2                       = Validator.mustBe[Int](_ => "ERROR < 2")(_ < 2)
    val graterThen0                     = Validator.mustBe[Int](_ => "ERROR > 0")(_ > 0)
    val between_0_and_2: Validator[Int] = lessThan2 && graterThen0

    assertEquals(
      obtained = between_0_and_2(1),
      expected = Valid(1)
    )

    assertEquals(
      obtained = between_0_and_2(2),
      expected = Invalid(NonEmptyList.one("ERROR < 2"))
    )

    assertEquals(
      obtained = between_0_and_2(0),
      expected = Invalid(NonEmptyList.one("ERROR > 0"))
    )
  }

  test(s"Validator.or") {

    val isEven                               = Validator.mustBe[Int](_ => "ERROR EVEN")(_ % 2 == 0)
    val graterThen_2                         = Validator.mustBe[Int](_ => "ERROR > 10")(_ > 10)
    val graterThan_10_OrEven: Validator[Int] = isEven || graterThen_2

    // even and > 10
    assertEquals(
      obtained = graterThan_10_OrEven(20),
      expected = Valid(20)
    )

    // even but not > 10
    assertEquals(
      obtained = graterThan_10_OrEven(2),
      expected = Valid(2)
    )

    // odd and NOT > 10
    assertEquals(
      obtained = graterThan_10_OrEven(7),
      expected = Invalid(NonEmptyList.of("ERROR EVEN", "ERROR > 10"))
    )
  }

  property(s"Validator.alwaysValid") {
    forAll { (value: Int) =>
      assertEquals(
        obtained = Validator.alwaysValid[Int](value),
        expected = Valid(value)
      )
    }
  }

  property(s"Validator.alwaysInvalid") {
    forAll { (value: Int) =>
      assertEquals(
        obtained = Validator.alwaysInvalid[Int](t => s"$t ERROR")(value),
        expected = Invalid(NonEmptyList.one(s"$value ERROR"))
      )
    }
  }
}
