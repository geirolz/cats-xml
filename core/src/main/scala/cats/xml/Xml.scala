package cats.xml

import cats.{Eq, Show}
import cats.xml.utils.{impure, Debug, UnsafeValidator}
import cats.xml.XmlData.*
import cats.xml.codec.Decoder

import scala.reflect.ClassTag
import scala.util.Try

trait Xml {

  final lazy val isNull: Boolean = this match {
    case Xml.Null => true
    case _        => false
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

  final lazy val Null: Xml & XmlNode & XmlData = XmlNull
  final lazy val True: XmlBool                 = ofBoolean(true)
  final lazy val False: XmlBool                = ofBoolean(false)
  final lazy val emptyString: XmlString        = ofString("")

  def ofString(value: String): XmlString                        = XmlString(value)
  def ofChar(value: Char): XmlChar                              = XmlChar(value)
  def ofBoolean(value: Boolean): XmlBool                        = XmlBool(value)
  def ofByte(value: Byte): XmlNumber                            = XmlByte(value)
  def ofShort(value: Short): XmlNumber                          = XmlShort(value)
  def ofInt(value: Int): XmlNumber                              = XmlInt(value)
  def ofLong(value: Long): XmlNumber                            = XmlLong(value)
  def ofFloat(value: Float): XmlNumber                          = XmlFloat(value)
  def ofDouble(value: Double): XmlNumber                        = XmlDouble(value)
  def ofBigInt(value: BigInt): XmlNumber                        = XmlBigInt(value)
  def ofBigDecimal(value: BigDecimal): XmlNumber                = XmlBigDecimal(value)
  def ofArray[T <: XmlData](value: Array[T]): XmlArray[T]       = XmlArray(value)
  def ofSeq[T <: XmlData: ClassTag](value: Seq[T]): XmlArray[T] = XmlArray(value.toArray)
  def ofValues[T <: XmlData: ClassTag](value: T*): XmlArray[T]  = XmlArray(value.toArray)
  def fromNumberString(str: String): Option[XmlNumber] = {
    Try(BigDecimal.exact(str)).toOption.map {
      case db if db.isValidByte     => ofByte(db.toByteExact)
      case db if db.isValidShort    => ofShort(db.toShortExact)
      case db if db.isValidInt      => ofInt(db.toIntExact)
      case bd if bd.isValidLong     => ofLong(bd.toLongExact)
      case db if db.isDecimalFloat  => ofFloat(db.floatValue)
      case db if db.isDecimalDouble => ofDouble(db.doubleValue)
      case bd =>
        bd.toBigIntExact match {
          case Some(bi) => ofBigInt(bi)
          case None     => ofBigDecimal(bd)
        }
    }
  }

  def fromDataString(value: String): XmlData = {
    fromNumberString(value)
      .getOrElse {
        val strData = Xml.ofString(value)
        Decoder
          .oneOf(
            Decoder.decodeBoolean.map(Xml.ofBoolean),
            Decoder.decodeChar.map(Xml.ofChar)
          )
          .decode(strData)
          .getOrElse(strData)
      }
  }

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
