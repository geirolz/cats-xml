package cats.xml

import cats.{Endo, Eq, Show}
import cats.data.NonEmptyList
import cats.kernel.Monoid
import cats.xml.codec.{DataEncoder, Decoder}
import cats.xml.XmlNode.XmlNodeGroup

import scala.annotation.tailrec

sealed trait AbstractXmlNode extends Xml {

  // must be 'def' due it's mutable
  def content: NodeContent

  // must be 'def' due it's mutable
  def isEmpty: Boolean = content.isEmpty

  lazy val isGroup: Boolean = this match {
    case _: XmlNodeGroup => true
    case _: XmlNode      => false
  }

  def children: Seq[XmlNode] =
    content.children

  // find
  def findChild(thatLabel: String): Option[XmlNode] =
    findChildBy(_.label == thatLabel)

  def findChildBy(p: XmlNode => Boolean): Option[XmlNode] =
    children.find(p)

  def findDeepChild(thatLabel: String): Option[XmlNode] =
    findDeepChildBy(_.label == thatLabel)

  def findDeepChildBy(p: XmlNode => Boolean): Option[XmlNode] =
    deepSubNodes.find(p)

  // filter
  def filterChildren(thatLabel: String): List[XmlNode] =
    filterChildrenBy(_.label == thatLabel)

  def filterChildrenBy(p: XmlNode => Boolean): List[XmlNode] =
    children.filter(p).toList

  def filterDeepChildren(thatLabel: String): LazyList[XmlNode] =
    filterDeepChildrenBy(_.label == thatLabel)

  def filterDeepChildrenBy(p: XmlNode => Boolean): LazyList[XmlNode] =
    deepSubNodes.filter(p)

  def deepSubNodes: LazyList[XmlNode] = {

    @tailrec
    def rec(left: List[XmlNode], acc: LazyList[XmlNode]): LazyList[XmlNode] =
      left match {
        case Nil          => acc
        case head :: tail => rec(tail, acc.appended(head).lazyAppendedAll(head.deepSubNodes))
      }

    content.children match {
      case Nil                 => LazyList.empty
      case currentNodeChildren => rec(currentNodeChildren, LazyList.empty)
    }
  }
}

// --------------------- XML NODE ---------------------
sealed class XmlNode private (
  private var mLabel: String,
  private var mAttributes: List[XmlAttribute],
  private var mContent: NodeContent
) extends AbstractXmlNode {

  def label: String = mLabel

  def attributes: List[XmlAttribute] = mAttributes

  def content: NodeContent = mContent

  // ---- LABEL ----
  def updateLabel(newLabel: String): XmlNode =
    updateLabel(_ => newLabel)

  def updateLabel(f: Endo[String]): XmlNode =
    copy(label = f(label))

  // ---- ATTRS ----
  def findAttr(key: String): Option[XmlAttribute] =
    attributes.find(_.key == key)

  def findAttrValue(key: String): Option[String] =
    attributes
      .find(_.key == key)
      .map(_.value.asString)

  def findAttrValue[T: Decoder](key: String): Option[T] =
    attributes
      .find(_.key == key)
      .flatMap(_.value.as[T].toOption)

  def existsAttrValue[T: Decoder](key: String, p: T => Boolean): Boolean =
    findAttrValue[T](key).exists(p)

  def withAttributes(attrs: Seq[XmlAttribute]): XmlNode =
    updateAttrs(_ => attrs.toList)

  def withAttributes(attr: XmlAttribute, attrs: XmlAttribute*): XmlNode =
    updateAttrs(_ => (attr +: attrs).toList)

  def prependAttr(newAttr: XmlAttribute): XmlNode =
    updateAttrs(ls => newAttr +: ls)

  def appendAttr(newAttr: XmlAttribute): XmlNode =
    updateAttrs(ls => ls :+ newAttr)

  def removeAttr(key: String): XmlNode =
    updateAttrs(_.filterNot(_.key == key))

  def updateAttrs(f: Endo[List[XmlAttribute]]): XmlNode =
    copy(attributes = f(attributes))

  def updateAttr(key: String)(f: Endo[XmlAttribute]): XmlNode =
    updateAttrs(_.map(attr => if (attr.key == key) f(attr) else attr))

  // ------ CONTENT ------
  def hasChildren: Boolean = children.nonEmpty

  def hasText: Boolean = text.nonEmpty

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

  // -----------------------------//
  def copy(
    label: String                  = this.label,
    attributes: List[XmlAttribute] = this.attributes,
    content: NodeContent           = this.content
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

object XmlNode extends XmlNodeInstances {

  lazy val emptyGroup: XmlNodeGroup = group(Nil)

  def apply(
    label: String,
    attributes: List[XmlAttribute] = Nil,
    content: NodeContent           = NodeContent.empty
  ): XmlNode = new XmlNode(label, attributes, content)

  def group(node: XmlNode, nodeN: XmlNode*): XmlNodeGroup =
    group(node +: nodeN)

  def group(childrenSeq: Seq[XmlNode]): XmlNodeGroup =
    new XmlNodeGroup(childrenSeq)

  // ------------------ XML NODE GROUP -------------------
  final class XmlNodeGroup private[XmlNode] (childrenSeq: Seq[XmlNode])
      extends XmlNode("", Nil, NodeContent.childrenOrEmpty(childrenSeq)) {

    def toNode(label: String, attributes: List[XmlAttribute] = Nil): XmlNode =
      XmlNode(label, attributes, content)
  }
}

private[xml] sealed trait XmlNodeInstances {

  implicit val monoidXmlNodeGroup: Monoid[XmlNode] = new Monoid[XmlNode] {
    override def empty: XmlNode = XmlNode.emptyGroup
    override def combine(x: XmlNode, y: XmlNode): XmlNode =
      (x, y) match {
        case (x1: XmlNodeGroup, x2: XmlNodeGroup) =>
          XmlNode.group(x1.children ++ x2.children)
        case (x1: XmlNode, x2: XmlNodeGroup) =>
          XmlNode.group(x1 +: x2.children)
        case (x1: XmlNodeGroup, x2: XmlNode) =>
          XmlNode.group(x1.children :+ x2)
        case (x1: XmlNode, x2: XmlNode) =>
          XmlNode.group(Seq(x1, x2))
      }
  }

  implicit val eqXmlNode: Eq[XmlNode] =
    (x: XmlNode, y: XmlNode) =>
      x.label == y.label &&
        x.attributes == y.attributes &&
        x.content == y.content

  implicit def showXmlNode(implicit config: XmlPrinter.Config): Show[XmlNode] =
    XmlPrinter.prettyString(_)
}
