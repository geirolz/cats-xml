package cats.xml.validator

import cats.data.{NonEmptyList, ValidatedNel}
import cats.data.Validated.{Invalid, Valid}
import cats.kernel.Monoid
import cats.{Contravariant, Eq, Show}
import cats.xml.validator.Validator.must

import scala.util.matching.Regex

trait Validator[T] { $this =>

  def apply(t: T): Validator.Result[T]

  def rewordError(error: T => String): Validator[T] =
    Validator.of(t =>
      $this(t) match {
        case Valid(a)   => Valid(a)
        case Invalid(_) => Invalid(NonEmptyList.one(error(t)))
      }
    )

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

  def mustNot[T](errorMsg: T => String)(f: T => Boolean): Validator[T] =
    must(errorMsg)(f.andThen(r => !r))

  def must[T](errorMsg: T => String)(f: T => Boolean): Validator[T] =
    (t: T) =>
      if (f(t)) Valid(t)
      else Invalid(NonEmptyList.one(errorMsg(t)))

  def alwaysValid[T]: Validator[T] = of[T](Valid(_))

  def alwaysInvalid[T](error: T => String): Validator[T] = must[T](error(_))(_ => false)
}

private[validator] sealed trait ValidatorBuilders {

  import cats.implicits.*

  // ------------- generic -------------
  def eqvTo[T: Eq](that: T): Validator[T] =
    must[T](tis => s"Value '$tis' is NOT equivalent to '$that'.")(Eq[T].eqv(_, that))

  def notEqvTo[T: Eq](that: T): Validator[T] =
    must[T](tis => s"Value '$tis' expected to be NOT equivalent to '$that'.")(Eq[T].neqv(_, that))

  // ------------- numeric -------------
  def min[N](min: N, exclusive: Boolean = false)(implicit N: Numeric[N]): Validator[N] =
    exclusive match {
      case true  => must[N](t => s"Value '$t' is NOT > '$min'")(N.gt(_, min))
      case false => must[N](t => s"Value '$t' is NOT >= '$min'")(N.gteq(_, min))
    }

  def max[N](max: N, exclusive: Boolean = false)(implicit N: Numeric[N]): Validator[N] =
    exclusive match {
      case true  => must[N](t => s"Value '$t' is NOT < '$max'")(N.lt(_, max))
      case false => must[N](t => s"Value '$t' is NOT <= '$max'")(N.lteq(_, max))
    }

  // min < x < max
  def range[N](
    min: N,
    max: N,
    minExclusive: Boolean = false,
    maxExclusive: Boolean = false
  )(implicit N: Numeric[N]): Validator[N] =
    Monoid
      .combine(
        Validator.min[N](min, minExclusive),
        Validator.max(max, maxExclusive)
      )
      .rewordError(t => {
        val minSymbol = if (minExclusive) "<" else "<="
        val maxSymbol = if (maxExclusive) "<" else "<="
        s"Value '$t' is NOT in range [$min $minSymbol x $maxSymbol $max]"
      })

  def positive[N](implicit N: Numeric[N]): Validator[N]       = min(N.zero, exclusive = true)
  def positiveOrZero[N](implicit N: Numeric[N]): Validator[N] = min(N.zero)

  def negative[N](implicit N: Numeric[N]): Validator[N]       = max(N.zero, exclusive = true)
  def negativeOrZero[N](implicit N: Numeric[N]): Validator[N] = max(N.zero)

  // ------------- string -------------
  def emptyString: Validator[String] =
    must[String](str => s"Value '$str', expected to be empty.")(
      _.isEmpty
    )
  def nonEmptyString: Validator[String] =
    must[String](str => s"Value '$str', expected to be NON empty.")(
      _.nonEmpty
    )

  def exactLength(expected: Int): Validator[String] =
    eqvTo[Int](expected)
      .contramap[String](_.length)
      .rewordError(str => s"Length of '$str', expected $expected but is ${str.length}.")

  def maxLength(maxLen: Int): Validator[String] =
    max[Int](maxLen)
      .contramap[String](_.length)
      .rewordError(str => s"Length of '$str' expected to be <= $maxLen but is ${str.length}.")

  def minLength(minLen: Int): Validator[String] =
    min[Int](minLen)
      .contramap[String](_.length)
      .rewordError(str => s"Length of '$str' expected to be >= $minLen but is ${str.length}.")

  def regex(regex: Regex): Validator[String] =
    must[String](str => s"String '$str' doesn't match regex `$regex`.")(
      regex.matches(_)
    )

  // ------------- collections -------------
  def isEmpty[F[X] <: IterableOnce[X]]: Validator[F[Any]] =
    must[F[Any]](seq => s"${iterableToStr(seq)} is not empty.")(
      _.iterator.isEmpty
    )

  def nonEmpty[F[X] <: IterableOnce[X]]: Validator[F[Any]] =
    must[F[Any]](seq => s"${iterableToStr(seq)} is empty.")(
      _.iterator.nonEmpty
    )

  def maxSize[F[X] <: IterableOnce[X]](maxSize: Int): Validator[F[Any]] =
    must[F[Any]](seq => s"${iterableToStr(seq)} size must be <= $maxSize")(
      _.iterator.size <= maxSize
    )

  def minSize[F[X] <: IterableOnce[X]](minSize: Int): Validator[F[Any]] =
    must[F[Any]](seq => s"${iterableToStr(seq)} size must be >= $minSize")(
      _.iterator.size >= minSize
    )

  // ------------- cats-collections -------------
  def maxSizeNel[T: Show](maxSize: Int): Validator[NonEmptyList[T]] =
    must[NonEmptyList[T]](seq =>
      s"NonEmptyList${iterableToStr(seq.toList)} size must be <= $maxSize"
    )(
      _.size <= maxSize
    )

  def minSizeNel[T: Show](minSize: Int): Validator[NonEmptyList[T]] =
    must[NonEmptyList[T]](seq =>
      s"NonEmptyList${iterableToStr(seq.toList)} size must be >= $minSize"
    )(
      _.size >= minSize
    )

  private def iterableToStr[T](itOnce: IterableOnce[T], limit: Int = 10)(implicit
    s: Show[T]                                                     = Show.fromToString[T]
  ): String = {
    if (itOnce.iterator.size <= limit)
      itOnce.iterator.map(_.show).mkString("[", ", ", "]")
    else
      itOnce.iterator.take(limit).map(_.show).mkString("[", ", ", ",...]")
  }
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
