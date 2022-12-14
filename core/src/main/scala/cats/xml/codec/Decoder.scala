package cats.xml.codec

import cats.{Alternative, ApplicativeThrow, MonadError}
import cats.data.*
import cats.xml.*
import cats.xml.cursor.{Cursor, FreeCursor, NodeCursor}
import cats.xml.cursor.NodeCursor.Root
import cats.xml.Xml.XmlNull
import cats.xml.XmlData.*

import scala.collection.Factory
import scala.util.Try

trait Decoder[T] {

  import cats.implicits.*

  def decodeCursorResult(cursorResult: Cursor.Result[Xml]): Decoder.Result[T]

  def decode(xml: Xml): Decoder.Result[T] =
    decodeCursorResult(Right(xml))

  def map[U](f: T => U): Decoder[U] =
    flatMap(f.andThen(Decoder.pure(_)))

  def emap[U](f: T => Either[DecoderFailure, U]): Decoder[U] =
    flatMap(t => Decoder.const(f(t).toValidatedNel[DecoderFailure]))

  def emap[E, U](f: T => Either[E, U])(implicit ctx: E <:< Throwable): Decoder[U] =
    emap(f.andThen(_.leftMap(e => DecoderFailure.Error(e))))

  def emapTry[U](f: T => Try[U]): Decoder[U] =
    emap(f.andThen(_.toEither))

  def emapOption[U](f: T => Option[U]): Decoder[U] =
    emap(f.andThen(_.toRight(DecoderFailure.Custom("Empty option value"))))

  def flatMapF[U](f: T => Decoder.Result[U]): Decoder[U] =
    Decoder.of(ns => decodeCursorResult(ns).andThen(t => f(t)))

  def flatMap[U](f: T => Decoder[U]): Decoder[U] =
    Decoder.of(ns => decodeCursorResult(ns).andThen(t => f(t).decodeCursorResult(ns)))
}

object Decoder extends DecoderInstances with DecoderSyntax {

  import cats.implicits.*

  type Result[+T] = ValidatedNel[DecoderFailure, T]

  lazy val id: Decoder[Xml] = of {
    case Left(failure) => DecoderFailure.CursorFailed(failure).invalidNel
    case Right(value)  => value.validNel
  }

  def apply[T: Decoder]: Decoder[T] = implicitly[Decoder[T]]

  def of[T](f: Cursor.Result[Xml] => Decoder.Result[T]): Decoder[T] = f(_)

  def pure[T](t: => T): Decoder[T] =
    const(t.validNel)

  def failure[T](r: DecoderFailure): Decoder[T] =
    const(r.invalidNel)

  def const[T](r: => Decoder.Result[T]): Decoder[T] =
    Decoder.of(_ => r)

  def oneOf[T <: Any](
    d: Decoder[? <: T],
    d1: Decoder[? <: T],
    dn: Decoder[? <: T]*
  ): Decoder[? <: T] =
    Decoder.fromXml(xml => {
      NonEmptyList
        .of(d, (d1 +: dn)*)
        .foldLeft[Decoder.Result[T]](
          DecoderFailure.Custom("Cannot decode the value.").invalidNel[T]
        )((err, decoder) => {
          err match {
            case v @ Validated.Valid(_) => v
            case Validated.Invalid(errors1) =>
              decoder.decode(xml) match {
                case v @ Validated.Valid(_)     => v
                case Validated.Invalid(errors2) => errors2.concatNel(errors1).invalid[T]
              }
          }
        })
    })

  def fromCursor[U](
    f: NodeCursor => FreeCursor[Xml, U]
  ): Decoder[U] = Decoder.of {
    case Left(failure) => DecoderFailure.CursorFailed(failure).invalidNel
    case Right(xml: Xml) =>
      f(Root).focus(xml) match {
        case Validated.Valid(value) => value.validNel
        case Validated.Invalid(e)   => e.map(DecoderFailure.CursorFailed(_)).invalid
      }
  }

  def fromXml[T](f: Xml => Decoder.Result[T]): Decoder[T] =
    id.flatMapF(f)

  def fromOption[T](f: Xml => Option[T]): Decoder[T] =
    fromEither(f.andThen(_.toRight(DecoderFailure.Custom("Cannot decode None Xml."))))

  def fromEither[T](f: Xml => Either[DecoderFailure, T]): Decoder[T] =
    id.emap(f)

  def fromEither[E, T](f: Xml => Either[E, T])(implicit ctx: E <:< Throwable): Decoder[T] =
    id.emap(f)

  def fromTry[T](f: Xml => Try[T]): Decoder[T] =
    id.emapTry(f)
}

// #################### SYNTAX ####################
private[xml] trait DecoderSyntax {

  implicit class DecoderOps(xml: Xml) {
    def as[T](implicit d: Decoder[T]): Decoder.Result[T] =
      d.decode(xml)
  }
}

// ####################### INSTANCES #######################
private[xml] trait DecoderInstances extends DecoderDataInstances with DecoderLifterInstances {

  import cats.implicits.*

  implicit def codecToDecoder[T: Codec]: Decoder[T] = Codec[T].decoder

  implicit val monadErrorForDecoder: MonadError[Decoder, NonEmptyList[DecoderFailure]] =
    new MonadError[Decoder, NonEmptyList[DecoderFailure]] {

      override def raiseError[A](e: NonEmptyList[DecoderFailure]): Decoder[A] =
        Decoder.const(e.invalid)

      override def handleErrorWith[A](fa: Decoder[A])(
        f: NonEmptyList[DecoderFailure] => Decoder[A]
      ): Decoder[A] =
        Decoder.id.flatMap((xml: Xml) => {
          fa.decode(xml) match {
            case Validated.Valid(a)   => Decoder.pure(a)
            case Validated.Invalid(e) => f(e)
          }
        })

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

sealed private[xml] trait DecoderDataInstances {

  import cats.implicits.*

  implicit val decodeXml: Decoder[Xml] = Decoder.id
  implicit val decodeXmlData: Decoder[XmlData] = decodeXml.flatMapF {
    case data: XmlData => data.validNel
    case xml           => DecoderFailure.Custom(s"Unable to decode $xml to XmlData").invalidNel
  }
  implicit val decodeString: Decoder[String] = Decoder.id.flatMapF {
    case XmlAttribute(_, value) => decodeString.decode(value)
    case data: XmlData =>
      def rec(d: XmlData): String = d match {
        case XmlNull          => "null" // should never happen
        case XmlString(value) => value
        case XmlChar(value)   => value.toString
        case XmlBool(value)   => value.toString
        case n: XmlNumber[?]  => n.value.toString
        case XmlArray(value)  => value.map(rec).mkString(",")
      }

      rec(data).validNel
    case txtNode: XmlNode =>
      txtNode.text match {
        case Some(xmlData) => decodeString.decode(xmlData)
        case None          => DecoderFailure.NoTextAvailable(txtNode).invalidNel
      }
    case sbj => DecoderFailure.NoTextAvailable(sbj).invalidNel
  }
  implicit val decodeUnit: Decoder[Unit] = Decoder.pure[Unit](())
  implicit val decodeBoolean: Decoder[Boolean] = decodeString.map(_.toLowerCase).emap[Boolean] {
    case "true"  => Right(true)
    case "false" => Right(false)
    case v       => Left(DecoderFailure.CoproductNoMatch[Any](v, Vector(true, false, 1, 0)))
  }
  implicit val decodeCharArray: Decoder[Array[Char]] = decodeString.map(_.toCharArray)
  implicit val decodeChar: Decoder[Char] = decodeString.emap(s => {
    if (s.nonEmpty && s.length == 1)
      Right(s.head)
    else
      Left(DecoderFailure.Custom(s"Unable to decode string $s into `Char`"))
  })
  implicit val decodeByte: Decoder[Byte]             = decodeString.emapTry(s => Try(s.toByte))
  implicit val decodeInt: Decoder[Int]               = decodeString.emapTry(s => Try(s.toInt))
  implicit val decodeLong: Decoder[Long]             = decodeString.emapTry(s => Try(s.toLong))
  implicit val decodeFloat: Decoder[Float]           = decodeString.emapTry(s => Try(s.toFloat))
  implicit val decodeDouble: Decoder[Double]         = decodeString.emapTry(s => Try(s.toDouble))
  implicit val decodeBigDecimal: Decoder[BigDecimal] = decodeString.emapTry(s => Try(BigDecimal(s)))
  implicit val decodeBigInt: Decoder[BigInt]         = decodeString.emapTry(s => Try(BigInt(s)))
}

sealed private[xml] trait DecoderLifterInstances { this: DecoderDataInstances =>

  import cats.implicits.*

  implicit def decoderLiftToApplicativeThrow[F[_], T: Decoder](implicit
    F: ApplicativeThrow[F]
  ): Decoder[F[T]] =
    Decoder.of {
      case Right(value)  => Decoder[T].decode(value).map(_.pure[F])
      case Left(failure) => F.raiseError[T](failure.asException).validNel
    }

  implicit def decoderLiftToAlternative[F[_], T: Decoder](implicit
    F: Alternative[F]
  ): Decoder[F[T]] =
    Decoder.of {
      case Right(value) => Decoder[T].decode(value).map(_.pure[F])
      case Left(_)      => F.empty[T].validNel
    }

  implicit def decoderLiftToSeq[F[X] <: Seq[X], T: Decoder](implicit
    f: Factory[T, F[T]]
  ): Decoder[F[T]] =
    decodeString
      .flatMapF(str => {
        str
          .split(",")
          .map(s => Decoder[T].decode(Xml.Data.fromString(s)))
          .toVector
          .sequence
          .map(_.to(f))
      })

  implicit def decodeCatsNel[T: Decoder]: Decoder[NonEmptyList[T]] =
    decoderLiftToSeq[Vector, T].flatMapF {
      case xs if xs.isEmpty => DecoderFailure.Custom("a NonEmptyChain is required.").invalidNel
      case xs               => NonEmptyList.of(xs.head, xs.tail*).validNel
    }

  implicit def decodeCatsNec[T: Decoder]: Decoder[NonEmptyChain[T]] =
    decoderLiftToSeq[Vector, T].flatMapF {
      case xs if xs.isEmpty => DecoderFailure.Custom("a NonEmptyChain is required.").invalidNel
      case xs               => NonEmptyChain.of(xs.head, xs.tail*).validNel
    }

  implicit def decodeCatsNes[T: Decoder]: Decoder[NonEmptySeq[T]] =
    decoderLiftToSeq[Vector, T].flatMapF {
      case xs if xs.isEmpty => DecoderFailure.Custom("a NonEmptySeq is required.").invalidNel
      case xs               => NonEmptySeq.of(xs.head, xs.tail*).validNel
    }

  implicit def decodeCatsNev[T: Decoder]: Decoder[NonEmptyVector[T]] =
    decoderLiftToSeq[Vector, T].flatMapF {
      case xs if xs.isEmpty => DecoderFailure.Custom("a NonEmptySeq is required.").invalidNel
      case xs               => NonEmptyVector.of(xs.head, xs.tail*).validNel
    }
}
