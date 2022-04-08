package cats.xml

import cats.{Endo, Eq, Show}
import cats.data.NonEmptyList
import cats.xml.codec.DataEncoder

import scala.annotation.tailrec

class XmlNode private (
  private var mLabel: String,
  private var mAttributes: Seq[XmlAttribute],
  private var mContent: NodeContent
) extends Xml {

  def label: String = mLabel

  def attributes: Seq[XmlAttribute] = mAttributes

  def content: NodeContent = mContent

  // ---- LABEL ----
  def updateLabel(newLabel: String): XmlNode =
    updateLabel(_ => newLabel)

  def updateLabel(f: Endo[String]): XmlNode =
    copy(label = f(label))

  // ---- ATTRS ----
  def findAttr(key: String): Option[XmlAttribute] =
    attributes.find(_.key == key)

  def withAttributes(attrs: Seq[XmlAttribute]): XmlNode =
    updateAttrs(_ => attrs)

  def withAttributes(attr: XmlAttribute, attrs: XmlAttribute*): XmlNode =
    updateAttrs(_ => attr +: attrs)

  def prependAttr(newAttr: XmlAttribute): XmlNode =
    updateAttrs(ls => newAttr +: ls)

  def appendAttr(newAttr: XmlAttribute): XmlNode =
    updateAttrs(ls => ls :+ newAttr)

  def removeAttr(key: String): XmlNode =
    updateAttrs(_.filterNot(_.key == key))

  def updateAttrs(f: Endo[Seq[XmlAttribute]]): XmlNode =
    copy(attributes = f(attributes))

  def updateAttr(key: String)(f: Endo[XmlAttribute]): XmlNode =
    updateAttrs(_.map(attr => if (attr.key == key) f(attr) else attr))

  // ------ CONTENT ------
  val hasChildren: Boolean = children.nonEmpty

  val hasText: Boolean = text.nonEmpty

  val isEmpty: Boolean = content.isEmpty

  def withText[T: DataEncoder](data: T): XmlNode =
    withContent(NodeContent.text(data))

  def updateText[T: DataEncoder](f: Option[XmlData] => T): XmlNode =
    withText(f(text))

  def withChild(child: XmlNode, children: XmlNode*): XmlNode =
    withChildren(child +: children)

  def withChildren(children: Seq[XmlNode]): XmlNode =
    withContent(NodeContent.childrenSeq(children).getOrElse(NodeContent.empty))

  def updateChildren(f: Endo[Seq[XmlNode]]): XmlNode =
    updateContent(currentContent =>
      NonEmptyList.fromFoldable(f(currentContent.children)) match {
        case Some(newChildrenNel) => NodeContent.Children(newChildrenNel)
        case None                 => NodeContent.Empty
      }
    )

  def appendChild(child: XmlNode, children: XmlNode*): XmlNode =
    updateChildren(currentChildren => currentChildren ++ List(child) ++ children)

  def prependChild(child: XmlNode, children: XmlNode*): XmlNode =
    updateChildren(currentChildren => List(child) ++ children ++ currentChildren)

  def drainContent: XmlNode =
    withContent(NodeContent.empty)

  private[xml] def withContent(newContent: NodeContent): XmlNode =
    updateContent(_ => newContent)

  private[xml] def updateContent(f: Endo[NodeContent]): XmlNode =
    copy(content = f(content))

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

  def deepSubNodes: LazyList[XmlNode] = {

    @tailrec
    def rec(left: List[XmlNode], acc: LazyList[XmlNode]): LazyList[XmlNode] =
      left match {
        case Nil          => acc
        case head :: tail => rec(tail, acc ++ head.deepSubNodes)
      }

    content.children match {
      case Nil                 => LazyList.empty
      case currentNodeChildren => rec(currentNodeChildren, LazyList.empty)
    }
  }

  // -----------------------------//
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

  override def equals(obj: Any): Boolean =
    obj match {
      case that: XmlNode => Eq[XmlNode].eqv(this, that)
      case _             => false
    }

  override final lazy val toString: String =
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

  implicit val eqXmlNode: Eq[XmlNode] =
    (x: XmlNode, y: XmlNode) =>
      x.label == y.label &&
        x.attributes == y.attributes &&
        x.content == y.content

  implicit def showXmlTree(implicit config: XmlPrinter.Config): Show[XmlNode] =
    XmlPrinter.prettyString(_)
}
