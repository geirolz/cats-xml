package cats.xml

import org.w3c.dom.{Attr, Document as JDocument, NamedNodeMap, Node as JNode, NodeList}

import scala.annotation.tailrec

object JavaConverters extends JavaConvertersSyntax {

  def nodeFromJavaDocument(doc: JDocument): XmlNode = {

    @tailrec
    def recScanJNode(left: List[JNode], acc: XmlNode): XmlNode =
      left match {
        case Nil                                         => acc
        case head :: tail if head.getNodeName == "#text" => recScanJNode(tail, acc)
        case head :: tail => recScanJNode(tail, acc.withChild(rec(head)))
      }

    def rec(ns: JNode): XmlNode = {
      val baseNode: XmlNode = XmlNode(ns.getNodeName)
        .withAttributes(JavaConverters.attrFromJavaNodeMap(ns.getAttributes))

      if (ns.hasChildNodes) {
        val childNodes: NodeList = ns.getChildNodes
        recScanJNode(
          left = Iterator.tabulate(childNodes.getLength)(childNodes.item(_)).toList,
          acc  = baseNode
        )
      } else {
        baseNode.withText(ns.getTextContent)
      }
    }

    rec(doc.getDocumentElement)
  }

  def attrFromJavaNodeMap(nMap: NamedNodeMap): List[XmlAttribute] =
    if (nMap == null)
      Nil
    else {
      val len: Int                    = nMap.getLength
      val result: Array[XmlAttribute] = new Array[XmlAttribute](len)
      for (i <- 0 until nMap.getLength) {
        val item = nMap.item(i).asInstanceOf[Attr]
        result(i) = XmlAttribute(item.getName, item.getValue)
      }

      result.toList
    }
}

private sealed trait JavaConvertersSyntax {

  implicit class JDocumentOps(doc: JDocument) {
    def asCatsXml: XmlNode = JavaConverters.nodeFromJavaDocument(doc)
  }

  implicit class NamedNodeMapOps(nMap: NamedNodeMap) {
    def asCatsXml: List[XmlAttribute] = JavaConverters.attrFromJavaNodeMap(nMap)
  }
}
