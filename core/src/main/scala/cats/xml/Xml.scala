package cats.xml

import cats.MonadThrow
import org.w3c.dom.{Document as JDocument, Node as JNode, NodeList}

trait Xml
object Xml {

  def fromString[F[_]: MonadThrow](xmlString: String)(implicit parser: XmlParser[F]): F[XmlNode] =
    parser.parseString(xmlString)

  // TODO: JAVA support - to separate
  def fromJavaxDocument(doc: JDocument): XmlNode = {

    // TODO: NO STACK SAFE
    def rec(ns: JNode): XmlNode = {
      val baseNode: XmlNode = XmlNode(ns.getNodeName)
        .withAttributes(XmlAttribute.fromJavaNodeMap(ns.getAttributes))

      if (ns.hasChildNodes) {
        val childNodes: NodeList   = ns.getChildNodes
        val len: Int               = childNodes.getLength
        val result: Array[XmlNode] = new Array[XmlNode](len)
        for (i <- 0 until len) {
          result(i) = rec(childNodes.item(i))
        }

        baseNode.withChildren(result.toList.filterNot(_.label == "#text"))
      } else {
        baseNode.withText(ns.getTextContent)
      }
    }

    rec(doc.getDocumentElement)
  }
}
