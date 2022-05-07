package cats.xml

import cats.{Endo, Eq, Show}
import cats.xml.codec.DataEncoder

case class XmlAttribute(key: String, value: XmlData) extends Xml with Serializable {

  def map[T: DataEncoder](f: XmlData => T): XmlAttribute =
    map(value => DataEncoder[T].encode(f(value)))

  def map(f: Endo[XmlData]): XmlAttribute =
    XmlAttribute(key, f(value))

  override def toString: String = XmlAttribute.stringify(this)

  override def equals(obj: Any): Boolean =
    obj != null && obj
      .isInstanceOf[XmlAttribute] && Eq[XmlAttribute].eqv(this, obj.asInstanceOf[XmlAttribute])
}
object XmlAttribute extends XmlAttributeSyntax with XmlAttributeInstances {

  def apply[T: DataEncoder](key: String, value: T): XmlAttribute =
    XmlAttribute(key, DataEncoder[T].encode(value))

  def fromMap(values: Map[String, String]): List[XmlAttribute] =
    values.map { case (k, v) =>
      XmlAttribute(k, v)
    }.toList

  def stringify(ls: XmlAttribute)(implicit showData: Show[XmlData]): String =
    ls.value match {
      case Xml.Null => ""
      case _        => s"${ls.key}=\"${showData.show(ls.value)}\""
    }
}
private[xml] trait XmlAttributeSyntax {

  implicit class XmlAttrStringOps(key: String) {
    def :=[T: DataEncoder](value: T): XmlAttribute = XmlAttribute[T](key, value)
  }

  implicit class XmlAttrOps(attr: XmlAttribute) {

    def +(attr2: XmlAttribute): Seq[XmlAttribute] = Vector(attr, attr2)

    def ++(attrs: Seq[XmlAttribute]): Seq[XmlAttribute] = attr +: attrs
  }
}
private[xml] sealed trait XmlAttributeInstances {

  implicit def showInstanceForAttr(implicit sd: Show[XmlData]): Show[XmlAttribute] =
    attr => XmlAttribute.stringify(attr)

  implicit val eqForXmlAttribute: Eq[XmlAttribute] = (x: XmlAttribute, y: XmlAttribute) =>
    x.key == y.key && x.value == y.value
}
