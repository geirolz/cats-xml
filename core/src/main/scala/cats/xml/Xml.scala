package cats.xml

import cats.xml.XmlData.*
import cats.xml.utils.{impure, Debug, UnsafeValidator}
import cats.{Eq, Show}

import scala.reflect.ClassTag

trait Xml {

  final lazy val isNull: Boolean =
    this.isInstanceOf[XmlNull.type]

  final lazy val isData: Boolean =
    this.isInstanceOf[XmlData]

  final lazy val isAttribute: Boolean =
    this.isInstanceOf[XmlAttribute]

  final lazy val isNode: Boolean =
    this.isInstanceOf[XmlNode]

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

  override def equals(obj: Any): Boolean =
    obj match {
      case obj: Xml => Xml.eqXml.eqv(this, obj)
      case _        => false
    }

  override final def toString: String =
    Debug.ifEnabledAnd(_.doNotOverrideXmlToString)(
      ifTrue  = super.toString,
      ifFalse = Show[Xml].show(this)
    )
}
object Xml {

  import cats.syntax.all.*

  final lazy val Null: Xml & XmlNode & XmlData = XmlNull
  final lazy val True: XmlBool                 = boolean(true)
  final lazy val False: XmlBool                = boolean(false)
  final lazy val emptyString: XmlString        = string("")

  def data[T](value: T): XmlData =
    value match {
      case value: String     => string(value)
      case value: Char       => char(value)
      case value: Boolean    => boolean(value)
      case value: Byte       => byte(value)
      case value: Short      => short(value)
      case value: Int        => int(value)
      case value: Long       => long(value)
      case value: Float      => float(value)
      case value: Double     => double(value)
      case value: BigInt     => bigInt(value)
      case value: BigDecimal => bigDecimal(value)
      case value             => string(value.toString)
    }

  def data[F[X] <: Iterable[X], T <: XmlData: ClassTag](iterable: F[T]): XmlData =
    XmlArray(iterable.toArray[T])

  def string(value: String): XmlString         = XmlString(value)
  def char(value: Char): XmlChar               = XmlChar(value)
  def boolean(value: Boolean): XmlBool         = XmlBool(value)
  def byte(value: Byte): XmlNumber             = XmlLong(value.toLong)
  def short(value: Short): XmlNumber           = XmlLong(value.toLong)
  def int(value: Int): XmlNumber               = XmlLong(value.toLong)
  def long(value: Long): XmlNumber             = XmlLong(value)
  def float(value: Float): XmlNumber           = XmlFloat(value)
  def double(value: Double): XmlNumber         = XmlDouble(value)
  def bigInt(value: BigInt): XmlNumber         = XmlBigDecimal(BigDecimal(value))
  def bigDecimal(value: BigDecimal): XmlNumber = XmlBigDecimal(value)

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
        case (a: XmlAttribute, b: XmlAttribute) => a.eqv(b)
        case (a: XmlData, b: XmlData)           => a.eqv(b)
        case (a: XmlNode, b: XmlNode)           => a.eqv(b)
      }
}
