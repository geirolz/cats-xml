package cats.xml.std

import cats.xml.*

import scala.annotation.{tailrec, unused}
import scala.xml.*

private[xml] object NodeSeqInterop extends XmlToNodeSeqSyntax {

  import cats.xml.std.implicits.*

  def fromNodeSeq(ns: NodeSeq): XmlNode =
    ns match {
      case node: Document =>
        fromNodeSeq(node.docElem)
      case e: Text =>
        XmlNode(
          label      = e.label,
          attributes = XmlAttribute.fromMetaData(e.attributes),
          content    = NodeContent.Text(XmlString(e.text.trim))
        )
      case e: Elem =>
        val tree = XmlNode(
          e.label,
          XmlAttribute.fromMetaData(e.attributes)
        )

        val neChild = e.child
          .filterNot(c =>
            c.isInstanceOf[Atom[?]] && c.asInstanceOf[Atom[?]].data.toString.trim.isEmpty
          )
          .toArray
        val neChildLen = neChild.length

        val content = if (neChildLen > 0) {
          val head = neChild.head
          if (head.isAtom) {
            NodeContent.text(head.asInstanceOf[Atom[?]].data.toString.trim)
          } else {

            val res: Array[XmlNode] = new Array[XmlNode](neChildLen)
            for (idx <- 0 until neChildLen) {
              res.update(idx, fromNodeSeq(neChild(idx)))
            }

            NodeContent.childrenSeq(res.toList).getOrElse(NodeContent.empty)
          }

        } else NodeContent.empty

        tree.withContent(content)
    }

  def toNodeSeq(tree: XmlNode): NodeSeq = {

    @tailrec
    def rec(ls: List[XmlNode], acc: Seq[Node]): Seq[Node] =
      ls match {
        case ::(head, tail) => rec(tail, (acc :+ toNodeSeq(head)).flatten)
        case Nil            => acc
      }

    val content: Seq[Node] = tree.content match {
      case NodeContent.Text(data)            => new Atom[String](data.toString)
      case NodeContent.Children(childrenNel) => rec(childrenNel.toList, Nil)
      case NodeContent.Empty                 => Nil
    }

    Elem(
      prefix = null,
      label  = tree.label,
      attributes = tree.attributes
        .map(a => XmlAttribute.toMetaData(a))
        .foldLeft[MetaData](Null)(MetaData.concatenate),
      scope         = TopScope,
      minimizeEmpty = true,
      child         = content*
    )
  }
}
private[xml] trait XmlToNodeSeqSyntax {

  implicit class XmlOps(xml: XmlNode) {
    def toNodeSeq: NodeSeq =
      NodeSeqInterop.toNodeSeq(xml)
  }

  implicit class NodeSeqOps(ns: NodeSeq) {

    implicit def nodeSeqToXmlNode(ns: NodeSeq): XmlNode =
      Xml.fromNodeSeq(ns)

    def fromNodeSeq: XmlNode =
      NodeSeqInterop.fromNodeSeq(ns)
  }

  implicit class XmlObjOps(@unused xmlObj: Xml.type) {

    def fromNodeSeq(ns: NodeSeq): XmlNode =
      NodeSeqInterop.fromNodeSeq(ns)

    def toNodeSeq(tree: XmlNode): NodeSeq =
      NodeSeqInterop.toNodeSeq(tree)
  }
}
