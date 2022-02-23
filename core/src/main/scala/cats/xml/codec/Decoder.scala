package cats.xml.codec

import cats.xml.*
import cats.xml.codec.Decoder.Result
import cats.xml.cursor.{CursorResult, FreeCursor, NodeCursor}
import cats.xml.cursor.NodeCursor.Root
import cats.{Applicative, MonadError}
import cats.data.*
import cats.data.Validated.Invalid

import scala.collection.Factory
import scala.util.Try

trait Decoder[T] {

  import cats.implicits.*

  def decode(xml: Xml): Decoder.Result[T]

  def map[U](f: T => U): Decoder[U] =
    flatMap(f.andThen(Decoder.pure(_)))

  def emap[U](f: T => Either[DecodingFailure, U]): Decoder[U] =
    flatMap(t => Decoder.const(f(t).toValidatedNel[DecodingFailure]))

  def emap[E, U](f: T => Either[E, U])(implicit ctx: E <:< Throwable): Decoder[U] =
    emap(f.andThen(_.leftMap(e => DecodingFailure.error(e))))

  def emapTry[U](f: T => Try[U]): Decoder[U] =
    emap(f.andThen(_.toEither))

  def flatMapR[U](f: T => Decoder.Result[U]): Decoder[U] =
    Decoder.of(ns => decode(ns).andThen(t => f(t)))

  def flatMap[U](f: T => Decoder[U]): Decoder[U] =
    Decoder.of(ns => decode(ns).andThen(t => f(t).decode(ns)))

  def liftToOption: Decoder[Option[T]] = map(Some(_))
}

object Decoder extends DecoderInstances {

  import cats.implicits.*
  type Result[T]     = ValidatedNel[DecodingFailure, T]
  type InvalidResult = Invalid[NonEmptyList[DecodingFailure]]
  object Result {
    def success[T](t: T): Result[T]              = t.validNel
    def failed[T](f: DecodingFailure): Result[T] = f.invalidNel
  }

  lazy val id: Decoder[Xml] = of(Result.success)

  def apply[T: Decoder]: Decoder[T] = implicitly[Decoder[T]]

  def of[T](f: Xml => Decoder.Result[T]): Decoder[T] = (xml: Xml) => f(xml)

  def pure[T](t: => T): Decoder[T] =
    const(Result.success(t))

  def failed[T](r: DecodingFailure): Decoder[T] =
    const(Result.failed(r))

  def const[T](r: => Decoder.Result[T]): Decoder[T] =
    Decoder.of(_ => r)

  def fromCursor[T, U](
    f: NodeCursor => FreeCursor[U]
  ): Decoder[U] =
    Decoder.of { tree =>
      f(Root).focus(tree) match {
        case CursorResult.Focused(value) => Result.success(value)
        case failed: CursorResult.Failed => Result.failed(DecodingFailure.cursorFailure(failed))
      }
    }

  def fromEither[T](f: Xml => Either[DecodingFailure, T]): Decoder[T] =
    id.emap(f)

  def fromEither[E, T](f: Xml => Either[E, T])(implicit ctx: E <:< Throwable): Decoder[T] =
    id.emap(f)

  def fromTry[T](f: Xml => Try[T]): Decoder[T] =
    id.emapTry(f)
}

private[xml] trait DecoderInstances
    extends DecoderPrimitivesInstances
    with DecoderLifterInstances
    with DecoderCatsDataInstances {

  import cats.implicits.*

  implicit def monadErrorForDecoder[T]: MonadError[Decoder, NonEmptyList[DecodingFailure]] =
    new MonadError[Decoder, NonEmptyList[DecodingFailure]] {

      override def raiseError[A](e: NonEmptyList[DecodingFailure]): Decoder[A] =
        Decoder.const(e.invalid)

      override def handleErrorWith[A](fa: Decoder[A])(
        f: NonEmptyList[DecodingFailure] => Decoder[A]
      ): Decoder[A] =
        Decoder.id.flatMap(ns => fa.decode(ns).fold(f, pure))

      override def pure[A](x: A): Decoder[A] =
        Decoder.pure(x)

      override def flatMap[A, B](fa: Decoder[A])(f: A => Decoder[B]): Decoder[B] =
        fa.flatMap(f)

      override def tailRecM[A, B](a: A)(f: A => Decoder[Either[A, B]]): Decoder[B] =
        f(a).flatMap {
          case Left(aa)  => tailRecM(aa)(f)
          case Right(bb) => Decoder.pure(bb)
        }
    }
}

sealed private[xml] trait DecoderPrimitivesInstances {

  import cats.implicits.*

  implicit val decodeString: Decoder[String] = Decoder.of {
    case XmlAttribute(_, value) => decodeString.decode(value)
    case data: XmlData =>
      def rec(d: XmlData): String = d match {
        case XmlString(value) => value
        case XmlNumber(value) => value.toString
        case XmlArray(value)  => value.map(rec).mkString("(", ",", ")")
        case XmlByte(value)   => value.toString
      }

      rec(data).validNel
    case txtNode: XmlNode =>
      txtNode.text match {
        case Some(xmlData) => decodeString.decode(xmlData)
        case None          => DecodingFailure.noTextAvailable(txtNode).invalidNel
      }
    case sbj => DecodingFailure.noTextAvailable(sbj).invalidNel
  }
  implicit val decodeXml: Decoder[Xml]   = Decoder.id
  implicit val decodeUnit: Decoder[Unit] = Decoder.pure[Unit](())
  implicit val decodeBoolean: Decoder[Boolean] = decodeString.map(_.toLowerCase).emap[Boolean] {
    case "true" | "1"  => Right(true)
    case "false" | "0" => Right(false)
    case v             => Left(DecodingFailure.coproductUnmatch(v, Vector(true, false, 1, 0)))
  }
  implicit val decodeCharArray: Decoder[Array[Char]] = decodeString.map(_.toCharArray)
  implicit val decodeInt: Decoder[Int]               = decodeString.emapTry(s => Try(s.toInt))
  implicit val decodeLong: Decoder[Long]             = decodeString.emapTry(s => Try(s.toLong))
  implicit val decodeFloat: Decoder[Float]           = decodeString.emapTry(s => Try(s.toFloat))
  implicit val decodeDouble: Decoder[Double]         = decodeString.emapTry(s => Try(s.toDouble))
  implicit val decodeBigDecimal: Decoder[BigDecimal] =
    decodeString.emapTry(s => Try(BigDecimal(s)))
}

sealed private[xml] trait DecoderLifterInstances { this: DecoderPrimitivesInstances =>

  import cats.implicits.*

  implicit def decoderLiftToApplicative[F[_]: Applicative, T: Decoder]: Decoder[F[T]] =
    Decoder[T].map(Applicative[F].pure)

  implicit def decoderLiftToSeq[F[X] <: Seq[X], T: Decoder](implicit
    f: Factory[T, F[T]]
  ): Decoder[F[T]] =
    decodeString
      .flatMapR(str => {
        str
          .split(",")
          .map(s => Decoder[T].decode(XmlString(s)))
          .toVector
          .sequence
          .map(_.to(f))
      })
}

sealed private[xml] trait DecoderCatsDataInstances { this: DecoderLifterInstances =>

  implicit def decodeCatsNel[T: Decoder]: Decoder[NonEmptyList[T]] =
    decoderLiftToSeq[Vector, T].flatMapR {
      case xs if xs.isEmpty =>
        Result.failed(DecodingFailure.custom("a NonEmptyChain is required."))
      case xs => Result.success(NonEmptyList.of(xs.head, xs.tail*))
    }

  implicit def decodeCatsNec[T: Decoder]: Decoder[NonEmptyChain[T]] =
    decoderLiftToSeq[Vector, T].flatMapR {
      case xs if xs.isEmpty => Result.failed(DecodingFailure.custom("a NonEmptyChain is required."))
      case xs               => Result.success(NonEmptyChain.of(xs.head, xs.tail*))
    }

  implicit def decodeCatsNes[T: Decoder]: Decoder[NonEmptySeq[T]] =
    decoderLiftToSeq[Vector, T].flatMapR {
      case xs if xs.isEmpty => Result.failed(DecodingFailure.custom("a NonEmptySeq is required."))
      case xs               => Result.success(NonEmptySeq.of(xs.head, xs.tail*))
    }

  implicit def decodeCatsNev[T: Decoder]: Decoder[NonEmptyVector[T]] =
    decoderLiftToSeq[Vector, T].flatMapR {
      case xs if xs.isEmpty => Result.failed(DecodingFailure.custom("a NonEmptySeq is required."))
      case xs               => Result.success(NonEmptyVector.of(xs.head, xs.tail*))
    }
}
