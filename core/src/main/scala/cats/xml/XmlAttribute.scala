package cats.xml

import cats.{Eq, Show}
import cats.xml.codec.DataEncoder
import org.w3c.dom.{Attr, NamedNodeMap}

import scala.xml.{MetaData, Null}

case class XmlAttribute(key: String, value: XmlData) extends Xml with Serializable {

  override def toString: String = XmlAttribute.stringify(this)

  override def equals(obj: Any): Boolean =
    obj != null
      && obj.isInstanceOf[XmlAttribute]
      && Eq[XmlAttribute].eqv(this, obj.asInstanceOf[XmlAttribute])
}
object XmlAttribute extends XmlAttributeSyntax with XmlAttributeInstances {

  def apply[T: DataEncoder](key: String, value: T): XmlAttribute =
    XmlAttribute(key, DataEncoder[T].encode(value))

  // TODO: DEPENDS ON STD XML - MOVE TO PROPER MODULE
  def fromMetaData(metaData: MetaData): List[XmlAttribute] =
    metaData.iterator.map(m => XmlAttribute(m.key, XmlString(m.value.text))).toList

  def fromJavaNodeMap(nMap: NamedNodeMap): List[XmlAttribute] =
    if(nMap == null)
      Nil
    else {
      val len: Int = nMap.getLength
      val result: Array[XmlAttribute] = new Array[XmlAttribute](len)
      for (i <- 0 until  nMap.getLength) {
        val item = nMap.item(i).asInstanceOf[Attr]
        result(i) = XmlAttribute(item.getName, item.getValue)
      }

      result.toList
    }

  def fromMap(values: Map[String, String]): List[XmlAttribute] =
    values.map {
      case (k, v) => XmlAttribute(k, v)
    }.toList

  // TODO: DEPENDS ON STD XML - MOVE TO PROPER MODULE
  def toMetaData(attr: XmlAttribute): MetaData =
   new scala.xml.UnprefixedAttribute(attr.key, attr.value.toString, Null)

  def stringify(ls: XmlAttribute): String =
    s"${ls.key}=\"${ls.value}\""
}
private[xml] trait XmlAttributeSyntax {

  implicit class XmlAttrStringOps(key: String){
    def :=[T: DataEncoder](value: T): XmlAttribute = XmlAttribute[T](key, value)
  }

  implicit class XmlAttrOps(attr: XmlAttribute){
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
