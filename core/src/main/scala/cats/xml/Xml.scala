package cats.xml

import cats.effect.{Resource, Sync}
import org.w3c.dom.{Document as JDocument, Node as JNode, NodeList}

import java.io.{ByteArrayInputStream, File, FileInputStream, InputStream}
import java.nio.charset.{Charset, StandardCharsets}
import javax.xml.parsers.{DocumentBuilder, DocumentBuilderFactory}
import scala.annotation.tailrec
import scala.util.Try
import scala.xml.*

private[xml] trait Xml
object Xml {

  import cats.implicits.*

  // TODO: DEPENDS ON CATS-EFFECT - MOVE TO PROPER MODULE
  def fromString[F[_]: Sync](string: String, charset: Charset = StandardCharsets.UTF_8): F[Xml] =
    fromSource[F](new ByteArrayInputStream(string.getBytes(charset))).use(_.pure[F])

  // TODO: DEPENDS ON CATS-EFFECT - MOVE TO PROPER MODULE
  def loadFile[F[_]: Sync](file: File): Resource[F, Xml] =
    fromSource[F](new FileInputStream(file))

  // TODO: DEPENDS ON CATS-EFFECT - MOVE TO PROPER MODULE
  def loadFile[F[_]: Sync](name: String): Resource[F, Xml] =
    fromSource[F](new FileInputStream(name))

  // TODO: DEPENDS ON CATS-EFFECT - MOVE TO PROPER MODULE
  def fromSource[F[_]](inputSource: => InputStream)(implicit F: Sync[F]): Resource[F, Xml] = {

    def parse(inputSource: InputSource): Try[Xml] = Try {

      // Parser that produces DOM object trees from XML content
      val factory: DocumentBuilderFactory = DocumentBuilderFactory.newInstance()

      // Create DocumentBuilder with default configuration//Create DocumentBuilder with default configuration
      val builder: DocumentBuilder = factory.newDocumentBuilder

      // Parse the content to Document object
      Xml.fromJavaxDocument(builder.parse(inputSource))
    }

    Resource
      .fromAutoCloseable(F.delay(inputSource))
      .evalMap(is => F.fromTry(parse(new InputSource(is))))
  }

  // TODO: DEPENDS ON STD XML - MOVE TO PROPER MODULE - LOW PERFORMANCE
  def fromNodeSeq(ns: NodeSeq): XmlNode =
    ns match {
      case node: Document =>
        fromNodeSeq(node.docElem)
      case e: Text =>
        XmlNode(
          label      = e.label,
          attributes = XmlAttribute.fromMetaData(e.attributes),
          content    = NodeContent.Text(XmlString.fromScalaText(e))
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

  // TODO: DEPENDS ON STD XML - MOVE TO PROPER MODULE - LOW PERFORMANCE
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

  // FIXME: LOW PERFORMANCE
  def fromJavaxDocument(doc: JDocument): Xml = {

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
