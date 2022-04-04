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

    val validator: Validator[Int] = Validator.must[Int](t => s"Value $t failed!") {
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

    val validator: Validator[Int] = Validator.mustNot[Int](t => s"Value $t failed!") {
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

    val lessThan2                       = Validator.must[Int](_ => "ERROR < 2")(_ < 2)
    val graterThen0                     = Validator.must[Int](_ => "ERROR > 0")(_ > 0)
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

    val isEven                               = Validator.must[Int](_ => "ERROR EVEN")(_ % 2 == 0)
    val graterThen_2                         = Validator.must[Int](_ => "ERROR > 10")(_ > 10)
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

class ValidatorInstancesSuite extends munit.ScalaCheckSuite {

  test("Validator.min") {

    val minInclusive: Validator[Int] = Validator.min(1)
    assertEquals(
      obtained = minInclusive(1),
      expected = Valid(1)
    )

    assertEquals(
      obtained = minInclusive(0),
      expected = Invalid(NonEmptyList.one("Value '0' is NOT >= '1'"))
    )

    val minExclusive: Validator[Int] = Validator.min(1, exclusive = true)
    assertEquals(
      obtained = minExclusive(2),
      expected = Valid(2)
    )

    assertEquals(
      obtained = minExclusive(1),
      expected = Invalid(NonEmptyList.one("Value '1' is NOT > '1'"))
    )
  }

  test("Validator.max") {

    val maxInclusive: Validator[Int] = Validator.max(10)
    assertEquals(
      obtained = maxInclusive(10),
      expected = Valid(10)
    )

    assertEquals(
      obtained = maxInclusive(11),
      expected = Invalid(NonEmptyList.one("Value '11' is NOT <= '10'"))
    )

    val maxExclusive: Validator[Int] = Validator.max(10, exclusive = true)
    assertEquals(
      obtained = maxExclusive(5),
      expected = Valid(5)
    )

    assertEquals(
      obtained = maxExclusive(10),
      expected = Invalid(NonEmptyList.one("Value '10' is NOT < '10'"))
    )
  }

  test("Validator.range") {

    val rangeInclusive: Validator[Int] = Validator.range(5, 10)
    assertEquals(
      obtained = rangeInclusive(5),
      expected = Valid(5)
    )

    assertEquals(
      obtained = rangeInclusive(10),
      expected = Valid(10)
    )

    assertEquals(
      obtained = rangeInclusive(11),
      expected = Invalid(NonEmptyList.one("Value '11' is NOT in range [5 <= x <= 10]"))
    )

    assertEquals(
      obtained = rangeInclusive(4),
      expected = Invalid(NonEmptyList.one("Value '4' is NOT in range [5 <= x <= 10]"))
    )

    val rangeExclusive: Validator[Int] = Validator.range(
      min          = 5,
      max          = 10,
      minExclusive = true,
      maxExclusive = true
    )

    assertEquals(
      obtained = rangeExclusive(5),
      expected = Invalid(NonEmptyList.one("Value '5' is NOT in range [5 < x < 10]"))
    )

    assertEquals(
      obtained = rangeExclusive(10),
      expected = Invalid(NonEmptyList.one("Value '10' is NOT in range [5 < x < 10]"))
    )

    assertEquals(
      obtained = rangeExclusive(11),
      expected = Invalid(NonEmptyList.one("Value '11' is NOT in range [5 < x < 10]"))
    )

    assertEquals(
      obtained = rangeExclusive(4),
      expected = Invalid(NonEmptyList.one("Value '4' is NOT in range [5 < x < 10]"))
    )
  }
}
