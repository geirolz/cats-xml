package cats.xml

import cats.{Endo, Show}
import cats.xml.codec.DataEncoder

import scala.xml.NodeSeq

class XmlNode private (
  private var mLabel: String,
  private var mAttributes: Seq[XmlAttribute],
  private var mContent: NodeContent
) extends Xml
    with LabelOps
    with AttrsOps
    with ContentOps {

  def label: String = mLabel

  def attributes: Seq[XmlAttribute] = mAttributes

  def content: NodeContent = mContent

  // update ops
  override def updateLabel(f: Endo[String]): XmlNode =
    copy(label = f(label))

  override def updateAttrs(f: Endo[Seq[XmlAttribute]]): XmlNode =
    copy(attributes = f(attributes))

  def copy(
    label: String                 = this.label,
    attributes: Seq[XmlAttribute] = this.attributes,
    content: NodeContent          = this.content
  ): XmlNode = new XmlNode(label, attributes, content)

  /** ##### BE CAREFUL! ##### */
  private[xml] def mute(f: Endo[XmlNode]): Unit = {
    val n: XmlNode = f(this)
    this.mLabel      = n.label
    this.mAttributes = n.attributes
    this.mContent    = n.content
  }

  private[xml] override def updateContent(f: Endo[NodeContent]): XmlNode =
    copy(content = f(content))

  override final def toString: String =
    Show[XmlNode].show(this)
}

object XmlNode extends XmlTreeInstances {

  def apply(
    label: String,
    attributes: Seq[XmlAttribute] = Nil,
    content: NodeContent          = NodeContent.empty
  ): XmlNode = new XmlNode(label, attributes, content)
}

private[xml] sealed trait XmlTreeInstances {

  implicit def nodeSeqToXmlNode(ns: NodeSeq): XmlNode = Xml.fromNodeSeq(ns)

  implicit val showXmlTree: Show[XmlNode] = XmlPrinter.prettyString(_)
}

// ######################## OPS ########################
private[xml] sealed trait LabelOps { $this: XmlNode =>

  def updateLabel(newLabel: String): XmlNode =
    updateLabel(_ => newLabel)

  def updateLabel(f: Endo[String]): XmlNode
}

private[xml] sealed trait AttrsOps { $this: XmlNode =>

  // ---- GET ----
  def findAttr(key: String): Option[XmlAttribute] =
    attributes.find(_.key == key)

  // ---- UPDATE ----
  def withAttributes(attrs: Seq[XmlAttribute]): XmlNode =
    updateAttrs(_ => attrs)

  def withAttributes(attr: XmlAttribute, attrs: XmlAttribute*): XmlNode =
    updateAttrs(_ => attr +: attrs)

  def withAttributesMap(values: Map[String, String]): XmlNode =
    updateAttrs(_ => XmlAttribute.fromMap(values))

  def prependAttr(newAttr: XmlAttribute): XmlNode =
    updateAttrs(ls => newAttr +: ls)

  def appendAttr(newAttr: XmlAttribute): XmlNode =
    updateAttrs(ls => ls :+ newAttr)

  def removeAttr(key: String): XmlNode =
    updateAttrs(_.filterNot(_.key == key))

  def updateAttrs(f: Endo[Seq[XmlAttribute]]): XmlNode
}

private[xml] sealed trait ContentOps { $this: XmlNode =>

  // ------ PROPS ------
  val hasChildren: Boolean = children.nonEmpty

  val hasText: Boolean = text.nonEmpty

  val isEmpty: Boolean = content.isEmpty

  // ------ UPDATE ------
  // text
  def withText[T: DataEncoder](data: T): XmlNode =
    withContent(NodeContent.text(data))

  // child
  def withChild(child: XmlNode, children: XmlNode*): XmlNode =
    withChildren(child +: children)

  def withChildren(children: Seq[XmlNode]): XmlNode =
    withContent(NodeContent.childrenSeq(children).getOrElse(NodeContent.empty))

  // generic
  def drain: XmlNode =
    withContent(NodeContent.empty)

  private[xml] def withContent(newContent: NodeContent): XmlNode =
    updateContent(_ => newContent)

  private[xml] def updateContent(f: Endo[NodeContent]): XmlNode

  // ------  GET ------
  def text: Option[XmlData] =
    content.text

  def children: Seq[XmlNode] =
    content.children

  def findChild(thatLabel: String): Option[XmlNode] =
    findChildBy(_.label == thatLabel)

  def findChildBy(p: XmlNode => Boolean): Option[XmlNode] =
    children.find(p)

  def findDeepChild(thatLabel: String): Option[XmlNode] =
    deepSubNodes.find(_.label == thatLabel)

  def deepSubNodes: List[XmlNode] =
    content match {
      case NodeContent.Children(childrenNel) => childrenNel.toList.flatMap(_.deepSubNodes)
      case _                                 => List(this)
    }
}
