package cats.xml.cursor

import cats.ApplicativeError
import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.data.Validated.{Invalid, Valid}
import cats.xml.{Xml, XmlNode}
import cats.xml.codec.{Decoder, DecoderFailure}
import cats.xml.validator.Validator

/** `FreeCursor` represent a cursor with a free `O` type as result of the focusing and a free `I`
  * type as target of the focusing.
  *
  * @tparam I
  *   Input type of the `FreeCursor`
  * @tparam O
  *   Output type of the `FreeCursor`
  */
sealed trait FreeCursor[I, +O] extends Serializable { $this =>

  /** Apply the current cursor to the specified input of type `I`. This allows to select a precise
    * part of the input `I` tree.
    *
    * The method is pure and return a `Left` when the focusing fails
    *
    * @param input
    *   target of the cursor
    * @return
    *   `Right` when succeed `Left` when fail
    */
  def focus(input: I): FreeCursor.Result[O]

  /** Map the result of this cursor when succeed
    * @param f
    *   Function to map the result of this cursor
    * @tparam U
    *   Type of mapped value
    * @return
    *   A new `FreeCursor` which once applied, apply this cursor and then if succeed apply the
    *   function `f` in order to map the result
    */
  def map[U](f: O => U): FreeCursor[I, U] =
    FreeCursor.of($this.focus(_).map(f))

  /** Create a new [[FreeCursor]] where the output of this cursor is validated with the specified
    * [[Validator]]
    * @param validator
    *   [[Validator]] instance to validate the output of this cursor
    * @tparam OO
    *   Output type of the new validated [[FreeCursor]]
    * @return
    *   A new validated [[FreeCursor]]
    */
  def validate[OO >: O](validator: Validator[OO]): FreeCursor[I, OO] =
    FreeCursor.of((input: I) =>
      $this.focus(input).andThen(o =>
        validator(o)
          .leftMap(eNel => NonEmptyList.one(CursorFailure.ValidationsFailed("** UNKNOWN **", eNel)))
      )
    )
}
object FreeCursor extends FreeCursorInstances {

  import cats.implicits.*

  type Result[+T] = ValidatedNel[CursorFailure, T]

  def id[T]: FreeCursor[T, T] =
    FreeCursor.of(_.validNel)

  def pure[I, O](value: O): FreeCursor[I, O] =
    const(value.validNel)

  def failure[I, O](value: NonEmptyList[CursorFailure]): FreeCursor[I, O] =
    const(value.invalid)

  def const[I, O](result: FreeCursor.Result[O]): FreeCursor[I, O] =
    FreeCursor.of(_ => result)

  private[xml] def of[I, O](f: I => FreeCursor.Result[O]): FreeCursor[I, O] =
    new FreeCursor[I, O] {
      override def focus(input: I): Result[O] = f(input)
    }

  def apply[O: Decoder](
    cursor: Cursor[Xml]
  ): FreeCursor[Xml, O] =
    new FreeCursor[Xml, O] { $this =>
      override def focus(xml: Xml): FreeCursor.Result[O] = {

        val cursorResult: Cursor.Result[Xml] = xml match {
          case node: XmlNode => cursor.focus(node)
          case element =>
            Left(CursorFailure.InvalidTargetType(XmlNode.getClass, element.getClass))
        }

        Decoder[O].decodeCursorResult(cursorResult) match {
          case Valid(value) => value.validNel
          case Invalid(failures: NonEmptyList[DecoderFailure]) =>
            val cursorFailures: NonEmptyList[CursorFailure] = failures.toList
              .partitionEither {
                case DecoderFailure.CursorFailed(failure) => Left(failure)
                case other                                => Right(other)
              }
              .bimap(
                NonEmptyList.fromList,
                NonEmptyList
                  .fromList(_)
                  .nested
                  .map(CursorFailure.DecoderFailed(cursor.path, _))
                  .value
              ) match {
              case (Some(cursorFailsNel), None)   => cursorFailsNel
              case (None, Some(otherDecFailsNel)) => otherDecFailsNel
              case (Some(cursorFailsNel), Some(otherDecFailsNel)) =>
                cursorFailsNel ::: otherDecFailsNel

              // paradox
              case (None, None) =>
                NonEmptyList.of(CursorFailure.Custom("Empty decoding failures unexpected."))
            }

            cursorFailures.invalid
        }
      }

      override def validate[OO >: O](validator: Validator[OO]): FreeCursor[Xml, OO] =
        FreeCursor.of((input: Xml) =>
          $this.focus(input).andThen(o =>
            validator(o)
              .leftMap(eNel => NonEmptyList.one(CursorFailure.ValidationsFailed(cursor.path, eNel)))
          )
        )
    }
}

private[xml] trait FreeCursorInstances {

  implicit def applicativeErrorForFreeCursor[I]
    : ApplicativeError[FreeCursor[I, *], NonEmptyList[CursorFailure]] =
    new ApplicativeError[FreeCursor[I, *], NonEmptyList[CursorFailure]] {

      override def map[A, B](fa: FreeCursor[I, A])(f: A => B): FreeCursor[I, B] =
        fa.map(f)

      def pure[A](a: A): FreeCursor[I, A] =
        FreeCursor.pure(a)

      def ap[A, B](ff: FreeCursor[I, A => B])(fa: FreeCursor[I, A]): FreeCursor[I, B] =
        FreeCursor.of((input: I) => fa.focus(input).ap(ff.focus(input)))

      override def product[A, B](
        fa: FreeCursor[I, A],
        fb: FreeCursor[I, B]
      ): FreeCursor[I, (A, B)] =
        FreeCursor.of((input: I) => fa.focus(input).product(fb.focus(input)))

      override def unit: FreeCursor[I, Unit] = pure(())

      def handleErrorWith[A](
        fa: FreeCursor[I, A]
      )(f: NonEmptyList[CursorFailure] => FreeCursor[I, A]): FreeCursor[I, A] =
        FreeCursor.of((input: I) =>
          fa.focus(input) match {
            case Validated.Invalid(e)   => f(e).focus(input)
            case v @ Validated.Valid(_) => v
          }
        )

      def raiseError[A](e: NonEmptyList[CursorFailure]): FreeCursor[I, A] =
        FreeCursor.const(Validated.Invalid(e))
    }
}
