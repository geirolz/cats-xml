package cats.xml

import cats.MonadThrow
import org.w3c.dom.{Document as JDocument, Node as JNode, NodeList}

import scala.annotation.tailrec

trait Xml
object Xml {

  def fromString[F[_]: MonadThrow](xmlString: String)(implicit parser: XmlParser[F]): F[XmlNode] =
    parser.parseString(xmlString)

  // TODO: JAVA support - to separate
  def fromJavaxDocument(doc: JDocument): XmlNode = {

    @tailrec
    def recScanJNode(left: List[JNode], acc: XmlNode): XmlNode =
      left match {
        case Nil                                         => acc
        case head :: tail if head.getNodeName == "#text" => recScanJNode(tail, acc)
        case head :: tail => recScanJNode(tail, acc.withChild(rec(head)))
      }

    def rec(ns: JNode): XmlNode = {
      val baseNode: XmlNode = XmlNode(ns.getNodeName)
        .withAttributes(XmlAttribute.fromJavaNodeMap(ns.getAttributes))

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
}
