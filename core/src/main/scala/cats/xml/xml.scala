package cats.xml

import cats.{xml, Eq, Show}
import cats.xml.Xml.XmlNull
import cats.xml.codec.Decoder
import cats.xml.utils.{impure, Debug, UnsafeValidator}

import java.io.InputStream
import java.nio.charset.{Charset, StandardCharsets}
import scala.reflect.ClassTag

trait Xml {

  final lazy val isNull: Boolean = this match {
    case XmlNull => true
    case _       => false
  }

  final lazy val isData: Boolean =
    asData.isDefined

  final lazy val isAttribute: Boolean =
    asAttribute.isDefined

  final lazy val isNode: Boolean =
    asNode.isDefined

  final def asData: Option[XmlData] = this match {
    case value: XmlData => Some(value)
    case _              => None
  }

  final def asAttribute: Option[XmlAttribute] = this match {
    case value: XmlAttribute => Some(value)
    case _                   => None
  }

  final def asNode: Option[XmlNode.Node] = this match {
    case value: XmlNode.Node => Some(value)
    case _                   => None
  }

  final def asGroup: Option[XmlNode.Group] = this match {
    case value: XmlNode.Group => Some(value)
    case _                    => None
  }

  final def asString: String = Show[Xml].show(this)

  override final def toString: String =
    Debug.ifEnabledAnd(_.doNotOverrideXmlToString)(
      ifTrue  = super.toString,
      ifFalse = asString
    )
}
object Xml {

  import cats.syntax.all.*

  case object XmlNull extends Xml with XmlData

  final lazy val Null: Xml & XmlData = XmlNull
  final lazy val Data: XmlData.type  = xml.XmlData

  def fromString[F[_]: XmlParser](
    text: String,
    charset: Charset = StandardCharsets.UTF_8
  ): F[XmlNode] =
    XmlParser[F].parseString(text, charset)

  def parseInputStream[F[_]: XmlParser](inputStream: InputStream): F[XmlNode] =
    XmlParser[F].parseInputStream(inputStream)

  /*
   * https://www.w3.org/TR/REC-xml/#NT-NameChar
   * */
  def isValidXmlName(string: String): Boolean =
    string != null &&
      string.nonEmpty &&
      string.matches(
        "^[:A-Z_a-z\\u00C0\\u00D6\\u00D8-\\u00F6\\u00F8-\\u02ff\\u0370-\\u037d"
          + "\\u037f-\\u1fff\\u200c\\u200d\\u2070-\\u218f\\u2c00-\\u2fef\\u3001-\\ud7ff"
          + "\\uf900-\\ufdcf\\ufdf0-\\ufffd\\x10000-\\xEFFFF]"
          + "[:A-Z_a-z\\u00C0\\u00D6\\u00D8-\\u00F6"
          + "\\u00F8-\\u02ff\\u0370-\\u037d\\u037f-\\u1fff\\u200c\\u200d\\u2070-\\u218f"
          + "\\u2c00-\\u2fef\\u3001-\\udfff\\uf900-\\ufdcf\\ufdf0-\\ufffd\\-\\.0-9"
          + "\\u00b7\\u0300-\\u036f\\u203f-\\u2040]*\\Z"
      )

  @impure
  def unsafeRequireValidXmlName(value: String): String =
    UnsafeValidator.unsafeRequire(value, s"Invalid XML name [$value].")(isValidXmlName)

  implicit val showXml: Show[Xml] = {
    case attr: XmlAttribute => attr.show
    case data: XmlData      => data.show
    case node: XmlNode      => node.show
    case other              => other.toString
  }

  implicit val eqXml: Eq[Xml] =
    (x: Xml, y: Xml) =>
      (x, y) match {
        case (XmlNull, XmlNull)                 => true
        case (a: XmlAttribute, b: XmlAttribute) => a.eqv(b)
        case (a: XmlData, b: XmlData)           => a.eqv(b)
        case (a: XmlNode, b: XmlNode)           => a.eqv(b)
      }
}

sealed trait XmlData extends Xml with Serializable {

  def as[T: Decoder]: Decoder.Result[T] = Decoder[T].decode(this)

  def isEmpty: Boolean = this match {
    case XmlData.XmlString(value) => value.isEmpty
    case XmlData.XmlArray(value)  => value.isEmpty
    case _                        => false
  }
}
object XmlData {

  lazy val True: XmlData  = fromBoolean(true)
  lazy val False: XmlData = fromBoolean(false)
  lazy val empty: XmlData = fromString("")

  // ------------------------------------//
  def fromString(value: String): XmlData         = XmlString(value)
  def fromChar(value: Char): XmlData             = XmlChar(value)
  def fromBoolean(value: Boolean): XmlData       = XmlBool(value)
  def fromInt(value: Int): XmlData               = XmlInt(value)
  def fromLong(value: Long): XmlData             = XmlLong(value)
  def fromFloat(value: Float): XmlData           = XmlFloat(value)
  def fromDouble(value: Double): XmlData         = XmlDouble(value)
  def fromBigDecimal(value: BigDecimal): XmlData = XmlBigDecimal(value)
  def fromBigInt(value: BigInt): XmlData         = XmlBigInt(value)

  def parseString(value: String): XmlData = {
    val strData = XmlData.fromString(value)
    Decoder
      .oneOf(
        Decoder.decodeBoolean.map(XmlData.fromBoolean),
        Decoder.decodeInt.map(XmlData.fromInt),
        Decoder.decodeLong.map(XmlData.fromLong),
        Decoder.decodeFloat.map(XmlData.fromFloat),
        Decoder.decodeDouble.map(XmlData.fromDouble),
        Decoder.decodeBigInt.map(XmlData.fromBigInt),
        Decoder.decodeBigDecimal.map(XmlData.fromBigDecimal),
        Decoder.decodeChar.map(XmlData.fromChar)
      )
      .decode(strData)
      .getOrElse(strData)
  }

  // collections
  def fromArray[T <: XmlData](value: Array[T]): XmlData       = XmlArray(value)
  def fromSeq[T <: XmlData: ClassTag](value: Seq[T]): XmlData = XmlArray(value.toArray)
  def fromValues[T <: XmlData: ClassTag](value: T*): XmlData  = XmlArray(value.toArray)

  // ------------------------------------//
  private[xml] final case class XmlString(value: String) extends XmlData
  private[xml] final case class XmlChar(value: Char) extends XmlData
  private[xml] final case class XmlBool(value: Boolean) extends XmlData

  // number
  private[xml] sealed trait XmlNumber[@specialized(Int, Long, Float, Double) T] extends XmlData {
    val value: T
  }
  private[xml] final case class XmlInt(value: Int) extends XmlNumber[Int]
  private[xml] final case class XmlLong(value: Long) extends XmlNumber[Long]
  private[xml] final case class XmlFloat(value: Float) extends XmlNumber[Float]
  private[xml] final case class XmlDouble(value: Double) extends XmlNumber[Double]
  private[xml] final case class XmlBigDecimal(value: BigDecimal) extends XmlNumber[BigDecimal]
  private[xml] final case class XmlBigInt(value: BigInt) extends XmlNumber[BigInt]

  // collections
  private[xml] final case class XmlArray[T <: XmlData](value: Array[T]) extends XmlData

  // ------------------------------------//
  // TODO: TO CHECK EQ TO DECODE STRING
  implicit def showXmlData[T <: XmlData]: Show[T] = {
    case XmlNull          => ""
    case XmlString(value) => value
    case XmlChar(value)   => value.toString
    case XmlBool(value)   => value.toString
    case n: XmlNumber[?]  => n.value.toString
    case XmlArray(value)  => value.mkString(",")
  }

  implicit val eqXmlData: Eq[XmlData] = (x: XmlData, y: XmlData) =>
    (x, y) match {
      case (a, b) if a.isNull && b.isNull     => true
      case (a: XmlString, b: XmlString)       => a.value == b.value
      case (a: XmlChar, b: XmlChar)           => a.value == b.value
      case (a: XmlBool, b: XmlBool)           => a.value == b.value
      case (a: XmlNumber[?], b: XmlNumber[?]) => a.value == b.value
      case (a: XmlArray[?], b: XmlArray[?])   => a.value.sameElements(b.value)
      case (_, _)                             => false
    }
}
