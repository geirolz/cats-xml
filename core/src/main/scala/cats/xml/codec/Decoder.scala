package cats.xml.codec

import cats.{Alternative, ApplicativeThrow, MonadError}
import cats.data.*
import cats.xml.*
import cats.xml.cursor.{Cursor, FreeCursor, NodeCursor}
import cats.xml.cursor.NodeCursor.Root
import cats.xml.XmlData.{XmlNumber, *}

import scala.collection.Factory
import scala.reflect.ClassTag
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

  def or(other: Decoder[? <: T]): Decoder[T] =
    this.attempt.flatMap {
      case Left(_)  => other.widen[T]
      case Right(t) => Decoder.pure(t)
    }
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

  def oneOf[T](
    d: Decoder[? <: T],
    d1: Decoder[? <: T],
    dn: Decoder[? <: T]*
  ): Decoder[T] =
    (List(d, d1) ++ dn.toList)
      .map(_.widen[T])
      .reduceLeft(_ or _)

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

  def dataRecursive[T](f: XmlData => Decoder.Result[T]): Decoder[T] =
    instance {
      case data: XmlData          => f(data)
      case XmlAttribute(_, value) => dataRecursive(f).decode(value)
      case node: XmlNode =>
        node.text match {
          case Some(xmlData) => dataRecursive(f).decode(xmlData)
          case None          => DecoderFailure.NoTextAvailable(node).invalidNel
        }
      case sbj => DecoderFailure.NoTextAvailable(sbj).invalidNel
    }

  def numberRec[T: ClassTag](f: String => Option[T]): Decoder[T] =
    numberOrCharRec(f, char => DecoderFailure.UnableToDecodeType[T](char).invalidNel)

  def numberOrCharRec[T: ClassTag](
    ifNumberOrString: String => Option[T],
    ifChar: Char => Decoder.Result[T]
  ): Decoder[T] =
    dataRecursive {
      case n: XmlNumber =>
        ifNumberOrString(n.toString).toValidNel(DecoderFailure.UnableToDecodeType[T](n))
      case c: XmlChar => ifChar(c.value)
      case s: XmlString =>
        ifNumberOrString(s.value).toValidNel(DecoderFailure.UnableToDecodeType[T](s))
      case v => DecoderFailure.UnableToDecodeType[T](v).invalidNel
    }

  def instance[T](f: Xml => Decoder.Result[T]): Decoder[T] =
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

  implicit val decodeXml: Decoder[Xml]   = Decoder.id
  implicit val decodeUnit: Decoder[Unit] = Decoder.pure[Unit](())
  implicit val decodeXmlData: Decoder[XmlData] = Decoder.instance {
    case data: XmlData => data.validNel
    case xml           => DecoderFailure.UnableToDecodeType[XmlData](xml).invalidNel
  }

  implicit val decodeString: Decoder[String] = Decoder.dataRecursive { data =>
    def rec(d: XmlData): String = d match {
      case XmlNull          => "null" // should never happen
      case XmlString(value) => value
      case XmlChar(value)   => value.toString
      case XmlBool(value)   => value.toString
      case n: XmlNumber     => n.stringValue
      case XmlArray(value)  => value.map(rec).mkString(",")
    }

    rec(data).validNel
  }

  implicit val decodeBoolean: Decoder[Boolean] = Decoder.dataRecursive {
    case XmlBool(true) | XmlString("true") | XmlChar('1') =>
      true.validNel
    case n: XmlNumber if n.toFloat == 1f =>
      true.validNel
    case XmlBool(false) | XmlString("false") | XmlChar('0') =>
      false.validNel
    case n: XmlNumber if n.toFloat == 0f =>
      false.validNel
    case v => DecoderFailure.CoproductNoMatch[Any](v, Vector(true, false, 1, 0)).invalidNel
  }

  implicit val decodeCharArray: Decoder[Array[Char]] =
    decodeString.map(_.toCharArray)

  implicit val decodeChar: Decoder[Char] = Decoder.dataRecursive {
    case XmlChar(value)                                          => value.validNel
    case XmlString(value) if value.nonEmpty && value.length == 1 => value.head.validNel
    case v => DecoderFailure.UnableToDecodeType[Char](v).invalidNel
  }

  implicit val decodeByte: Decoder[Byte] = Decoder.numberRec(_.toByteOption)
  implicit val decodeShort: Decoder[Short] =
    Decoder.numberOrCharRec(_.toShortOption, _.toShort.validNel)
  implicit val decodeInt: Decoder[Int]   = Decoder.numberOrCharRec(_.toIntOption, _.toInt.validNel)
  implicit val decodeLong: Decoder[Long] = Decoder.numberRec(_.toLongOption)
  implicit val decodeFloat: Decoder[Float]   = Decoder.numberRec(_.toFloatOption)
  implicit val decodeDouble: Decoder[Double] = Decoder.numberRec(_.toDoubleOption)
  implicit val decodeBigInt: Decoder[BigInt] = Decoder.numberRec(str => Try(BigInt(str)).toOption)
  implicit val decodeBigDecimal: Decoder[BigDecimal] =
    Decoder.numberRec(str => Try(BigDecimal(str)).toOption)
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
    Decoder.instance {
      case XmlArray(value) =>
        value
          .map(Decoder[T].decode)
          .toVector
          .sequence
          .map(_.to(f))
      case group: XmlNode.Group =>
        group.children
          .map(Decoder[T].decode)
          .toVector
          .sequence
          .map(_.to(f))
      case other => Decoder[T].decode(other).map(Seq(_).to(f))
    }

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
