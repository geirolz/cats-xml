package cats.xml.std

import cats.xml.{XmlAttribute, XmlString}

import scala.annotation.unused
import scala.xml.{MetaData, Null}

private[xml] object XmlAttributeInterop {

  def fromMetaData(metaData: MetaData): List[XmlAttribute] =
    metaData.iterator.map(m => XmlAttribute(m.key, XmlString(m.value.text))).toList

  def toMetaData(attr: XmlAttribute): MetaData =
    new scala.xml.UnprefixedAttribute(attr.key, attr.value.toString, Null)
}

private[xml] trait XmlAttributeInteropSyntax {

  implicit class MetaDataOps(metadata: MetaData) {
    def toXml: List[XmlAttribute] =
      XmlAttributeInterop.fromMetaData(metadata)
  }

  implicit class XmlAttributeOps(attr: XmlAttribute) {
    def toMetaData: MetaData =
      XmlAttributeInterop.toMetaData(attr)
  }

  implicit class XmlAttributeObjOps(@unused obj: XmlAttribute.type) {

    def fromMetaData(metaData: MetaData): List[XmlAttribute] =
      XmlAttributeInterop.fromMetaData(metaData)

    def toMetaData(attr: XmlAttribute): MetaData =
      XmlAttributeInterop.toMetaData(attr)
  }
}
