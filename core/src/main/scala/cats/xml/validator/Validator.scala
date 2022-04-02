package cats.xml.validator

import cats.data.{NonEmptyList, ValidatedNel}
import cats.data.Validated.{Invalid, Valid}
import cats.kernel.Monoid
import cats.Contravariant
import cats.xml.validator.Validator.mustBe

trait Validator[T] { $this =>

  def apply(t: T): Validator.Result[T]

  def contramap[U](f: U => T): Validator[U] =
    (u: U) => $this(f(u)).map(_ => u)

  def or(v1: Validator[T], vn: Validator[T]*): Validator[T] = {

    def singleOr(val1: Validator[T], val2: Validator[T]): Validator[T] =
      (t: T) =>
        (val1(t), val2(t)) match {
          case (Invalid(eNel1), Invalid(eNel2)) => Invalid(eNel1 ::: eNel2)
          case _                                => Valid(t)
        }

    (List(v1) ++ vn).foldLeft(this)(singleOr(_, _))
  }

  def and(v1: Validator[T], vn: Validator[T]*): Validator[T] = {

    def singleAnd(val1: Validator[T], val2: Validator[T]): Validator[T] =
      (t: T) =>
        (val1(t), val2(t)) match {
          case (Valid(t), Valid(_))             => Valid(t)
          case (Invalid(eNel1), Invalid(eNel2)) => Invalid(eNel1 ::: eNel2)
          case (_, Invalid(eNel))               => Invalid(eNel)
          case (Invalid(eNel), _)               => Invalid(eNel)
        }

    (List(v1) ++ vn).foldLeft(this)(singleAnd(_, _))
  }
}
object Validator extends ValidatorBuilders with ValidatorInstances with ValidatorSyntax {

  type Result[+T] = ValidatedNel[String, T]

  def of[T](f: T => Validator.Result[T]): Validator[T] = (t: T) => f(t)

  def mustNotBe[T](errorMsg: T => String)(f: T => Boolean): Validator[T] =
    mustBe(errorMsg)(f.andThen(r => !r))

  def mustBe[T](errorMsg: T => String)(f: T => Boolean): Validator[T] =
    (t: T) =>
      if (f(t)) Valid(t)
      else Invalid(NonEmptyList.one(errorMsg(t)))

  def alwaysValid[T]: Validator[T] = of[T](Valid(_))

  def alwaysInvalid[T](error: T => String): Validator[T] = mustBe[T](error(_))(_ => false)
}

private[validator] sealed trait ValidatorBuilders {

  // ------------- numeric -------------
  def min[N](inclusiveMin: N)(implicit N: Numeric[N]): Validator[N] =
    mustBe[N](t => s"Value '$t' is NOT >= '$inclusiveMin'")(
      N.gteq(_, inclusiveMin)
    )
  def max[N](inclusiveMax: N)(implicit N: Numeric[N]): Validator[N] =
    mustBe[N](t => s"Value '$t' is NOT <= '$inclusiveMax'")(
      N.lteq(_, inclusiveMax)
    )
  def range[N](inclusiveMin: N, inclusiveMax: N)(implicit N: Numeric[N]): Validator[N] =
    mustBe[N](t => s"Value '$t' is NOT in range [$inclusiveMin - $inclusiveMax]")(n =>
      N.lteq(n, inclusiveMax) && N.gteq(n, inclusiveMin)
    )

  def positive[N](implicit N: Numeric[N]): Validator[N]       = min(N.one)
  def positiveOrZero[N](implicit N: Numeric[N]): Validator[N] = min(N.zero)

  def negative[N](implicit N: Numeric[N]): Validator[N]       = max(N.negate(N.one))
  def negativeOrZero[N](implicit N: Numeric[N]): Validator[N] = max(N.zero)

  // ------------- string -------------
  def isEmpty: Validator[String] =
    mustBe[String](str => s"Value '$str', expected to be empty.")(
      _.isEmpty
    )
  def nonEmpty: Validator[String] =
    mustBe[String](str => s"Value '$str', expected to be NON empty.")(
      _.nonEmpty
    )
  def lenEq(expected: Long): Validator[String] =
    mustBe[String](str => s"Length of '$str', expected $expected but got ${str.length}")(
      _.length == expected
    )
  def regex(regex: String): Validator[String] =
    mustBe[String](str => s"String '$str' doesn't match regex `$regex`")(
      _.matches(regex)
    )
}

private[xml] trait ValidatorInstances {

  implicit val validatorContravariant: Contravariant[Validator] =
    new Contravariant[Validator] {
      override def contramap[A, B](fa: Validator[A])(f: B => A): Validator[B] =
        fa.contramap(f)
    }

  implicit def validatorMonoid[T]: Monoid[Validator[T]] = new Monoid[Validator[T]] {
    override def empty: Validator[T]                                     = Validator.alwaysValid[T]
    override def combine(x: Validator[T], y: Validator[T]): Validator[T] = x.and(y)
  }
}

private[xml] trait ValidatorSyntax {

  implicit class ValidatorOps[T](validator: Validator[T]) {
    def &&(that: Validator[T]): Validator[T] = validator.and(that)
    def ||(that: Validator[T]): Validator[T] = validator.or(that)
  }
}
