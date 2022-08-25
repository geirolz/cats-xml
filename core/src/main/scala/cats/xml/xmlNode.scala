package cats.xml

import cats.{Endo, Eq, Show}
import cats.data.NonEmptyList
import cats.kernel.Monoid
import cats.xml.codec.{DataEncoder, Decoder}
import cats.xml.Xml.unsafeRequireValidXmlName
import cats.xml.utils.impure
import cats.xml.utils.UnsafeValidator.unsafeRequireNotNull

import scala.annotation.tailrec
import scala.util.Try

sealed trait XmlNode extends Xml {

  type Self <: XmlNode

  /** Check if the node is a group
    *
    * @return
    *   `true` if node is a group, `false` otherwise
    */
  lazy val isGroup: Boolean = this match {
    case _: XmlNode.Group => true
    case _: XmlNode.Node  => false
  }

  /** Get the node label value
    *
    * {{{
    *   <Foo></Foo> //foo
    * }}}
    *
    * @return
    *   node label string
    */
  def label: String

  /** Get the node label value
    *
    * {{{
    *   <Foo a="1" b="2" ></Foo> //a="1" b="2"
    * }}}
    *
    * @return
    *   list of node attributes
    */
  def attributes: List[XmlAttribute]

  /** Return the node content which can be:
    *   - [[NodeContent.Empty]]
    *   - [[NodeContent.Text]]
    *   - [[NodeContent.Children]]
    *
    * If you need a specific kind of content please use either [[XmlNode.text]], [[XmlNode.isEmpty]]
    * or [[XmlNode.children]] instead
    *
    * @return
    *   Node content instance
    */
  def content: NodeContent

  /** Check if the node has empty content
    *
    * @return
    *   `true` if node content is empty, `false` otherwise
    */
  def isEmpty: Boolean = content.isEmpty

  /** Check is the node has text
    *
    * @return
    *   `true` if the node has text content, `false` otherwise. Always `false` if node is
    *   [[XmlNode.Group]]
    */
  def hasText: Boolean = text.nonEmpty

  /** Get node text data
    * @return
    *   Node text if the content contains text. Always `None` if node is [[XmlNode.Group]]
    */
  def text: Option[XmlData] =
    content.text

  /** Get node text as string
    *
    * @return
    *   Node text if the content contains text. Always `None` if node is [[XmlNode.Group]]
    */
  def textString: String =
    content.text.map(_.asString).getOrElse("")

  // must be 'def' due it's mutable
  /** Check is the node has children
    *
    * @return
    *   `true` if the node has children, `false` otherwise
    */
  def hasChildren: Boolean = children.nonEmpty

  // must be 'def' due it's mutable
  def children: Seq[XmlNode] =
    content.children

  /** Create a new immutable instance with the same values of the current one
    *
    * @return
    *   A new instance with the same values of the current one
    */
  def duplicate: Self

  /** Convert the node to a group. If this instance already is a group it will be returned the same
    * instance.
    */
  final def toGroup: XmlNode.Group = this match {
    case node: XmlNode.Node   => XmlNode.group(node.children)
    case group: XmlNode.Group => group
  }

  /** Update current node content
    * @return
    *   Same content with the new specified content.
    */
  private[xml] def updateContent(f: Endo[NodeContent]): Self

  /* ################################################ */
  /* ############### !! BE CAREFUL !! ############### */
  /* ################################################ */
  @impure
  def unsafeNarrowNode: XmlNode.Node =
    this.asInstanceOf[XmlNode.Node]

  @impure
  def unsafeNarrowGroup: XmlNode.Group =
    this.asInstanceOf[XmlNode.Group]

  // private unsafe
  @impure
  private[xml] def unsafeMute(f: Endo[XmlNode]): Unit =
    (this, f(this)) match {
      case (src: XmlNode.Node, upd: XmlNode.Node) =>
        src.unsafeMutableCopycat(upd)
      case (src: XmlNode.Group, upd: XmlNode.Group) =>
        src.unsafeMutableCopycat(upd)
      case (src: XmlNode.Node, upd: XmlNode.Group) =>
        src.toGroup.unsafeMutableCopycat(upd)
      case (src: XmlNode.Group, upd: XmlNode.Node) =>
        src.toNode(upd.label, upd.attributes).unsafeMutableCopycat(upd)
    }
  /* ################################################ */
  /* ############### !! BE CAREFUL !! ############### */
  /* ################################################ */

  override final def equals(obj: Any): Boolean =
    obj match {
      case that: XmlNode => Eq[XmlNode].eqv(this, that)
      case _             => false
    }

  override final lazy val toString: String =
    Show[XmlNode].show(this)
}
object XmlNode extends XmlNodeInstances with XmlNodeSyntax {

  lazy val emptyGroup: XmlNode.Group =
    XmlNode.group(Nil)

  /** Unsafe create a new [[XmlNode.Node]]
    * @param label
    *   Node label value. Must be valid and non-empty. See [[Xml.isValidXmlName]]
    * @param attributes
    *   Not attributes, could be empty.
    * @param content
    *   Node content.
    * @return
    *   A new [[XmlNode.Node]] instance with the specified values
    * @throws IllegalArgumentException
    *   If label is not a valid xml name or attributes and content are null.
    */
  @impure
  def apply(
    label: String,
    attributes: List[XmlAttribute] = Nil,
    content: NodeContent           = NodeContent.empty
  ): XmlNode.Node =
    new Node(
      unsafeRequireValidXmlName(label),
      XmlAttribute.normalizeAttrs(unsafeRequireNotNull(attributes)),
      unsafeRequireNotNull(content)
    )

  /** Safely create a new [[XmlNode.Node]]
    * @param label
    *   Node label value. Must be valid and non-empty
    * @param attributes
    *   Not attributes, could be empty.
    * @param content
    *   Node content.
    * @return
    *   A new [[XmlNode.Node]] instance with the specified values
    */
  def safeApply(
    label: String,
    attributes: List[XmlAttribute] = Nil,
    content: NodeContent           = NodeContent.empty
  ): Either[Throwable, XmlNode.Node] =
    Try(XmlNode(label, attributes, content)).toEither

  /** Create a new [[XmlNode.Group]] instance with the specified [[XmlNode]]s
    *
    * @param node
    *   element
    * @param nodeN
    *   other elements
    * @return
    *   A new [[XmlNode.Group]] instance with the specified [[XmlNode]]s
    */
  def group(node: XmlNode, nodeN: XmlNode*): XmlNode.Group =
    XmlNode.group(node +: nodeN)

  /** Create a new [[XmlNode.Group]] instance with the specified [[XmlNode]]s
    * @param elements
    *   Group elements
    * @return
    *   A new [[XmlNode.Group]] instance with the specified [[XmlNode]]s
    */
  @impure
  def group(elements: Seq[XmlNode]): XmlNode.Group =
    new Group(unsafeRequireNotNull(NodeContent.childrenOrEmpty(elements)))

  // --------------------- XML NODE ---------------------
  /** Represent a simple single XML node.
    *
    * {{{
    * <Bar>
    *   <Foo a="1">
    *   <Foo a="2">
    *   <Foo a="3">
    * </Bar>
    * }}}
    */
  final class Node private[XmlNode] (
    private var mLabel: String,
    private var mAttributes: List[XmlAttribute],
    private var mContent: NodeContent
  ) extends XmlNode {

    override type Self = XmlNode.Node

    override def label: String = mLabel

    override def attributes: List[XmlAttribute] = mAttributes

    override def content: NodeContent = mContent

    override def duplicate: XmlNode.Node = safeCopy()

    @impure
    private[xml] def updateLabel(f: Endo[String]): XmlNode.Node =
      safeCopy(label = f(label))

    @impure
    private[xml] def updateAttrs(f: Endo[List[XmlAttribute]]): XmlNode.Node =
      safeCopy(attributes = f(attributes))

    @impure
    private[xml] def updateContent(f: Endo[NodeContent]): XmlNode.Node =
      safeCopy(content = f(content))

    @impure
    private[xml] def safeCopy(
      label: String                  = this.label,
      attributes: List[XmlAttribute] = this.attributes,
      content: NodeContent           = this.content
    ): XmlNode.Node =
      XmlNode(
        label      = label,
        attributes = XmlAttribute.normalizeAttrs(attributes),
        content    = content
      )

    // -----------------------------//
    /* ################################################ */
    /* ############### !! BE CAREFUL !! ############### */
    /* ################################################ */
    @impure
    private[xml] def unsafeMuteNode(f: Endo[XmlNode.Node]): Unit =
      unsafeMutableCopycat(f(this))

    @impure
    private[xml] def unsafeMutableCopycat(n: XmlNode.Node): Unit = {
      this.mLabel      = n.label
      this.mAttributes = n.attributes
      this.mContent    = n.content
    }
    /* ################################################ */
    /* ############### !! BE CAREFUL !! ############### */
    /* ################################################ */
  }

  // ----------------------------- XML GROUP -----------------------------
  /** Class that represent a group of nodes without a container node.
    *
    * You can convert a [[Group]] into [[Node]] using `XmlNodeGroup.toNode` method.
    * {{{
    * <Foo a="1">
    * <Foo a="2">
    * <Foo a="3">
    * }}}
    */
  final class Group private[XmlNode] (
    private var mContent: NodeContent
  ) extends XmlNode {

    override type Self = XmlNode.Group

    override val label: String = ""

    override val attributes: List[XmlAttribute] = Nil

    override def content: NodeContent = mContent

    override def duplicate: XmlNode.Group = safeCopy()

    /** Convert current instance to a [[XmlNode.Node]].
      *
      * Wrap the group with a new [[XmlNode]]
      *
      * {{{
      *
      * // before
      * <Foo a="1">
      * <Foo a="2">
      * <Foo a="3">
      *
      * // after
      * <Bar>
      *   <Foo a="1">
      *   <Foo a="2">
      *   <Foo a="3">
      * </Bar>
      * }}}
      *
      * @param label
      *   name of the wrapper node
      * @param attributes
      *   attributes of the wrapper node, could be empty
      * @return
      *   An [[XmlNode]] with contains the current group nodes as children.
      */
    @impure
    def toNode(label: String, attributes: List[XmlAttribute] = Nil): XmlNode.Node =
      XmlNode(label, attributes, content)

    @impure
    private[xml] def updateContent(f: Endo[NodeContent]): XmlNode.Group =
      safeCopy(content = f(content))

    @impure
    private[xml] def safeCopy(
      content: NodeContent = this.mContent
    ): XmlNode.Group = new Group(
      unsafeRequireNotNull(content)
    )

    // -----------------------------//
    /* ################################################ */
    /* ############### !! BE CAREFUL !! ############### */
    /* ################################################ */
    @impure
    private[xml] def unsafeMuteGroup(f: Endo[XmlNode.Group]): Unit =
      unsafeMutableCopycat(f(this))

    @impure
    private[xml] def unsafeMutableCopycat(n: XmlNode.Group): Unit =
      this.mContent = n.content
    /* ################################################ */
    /* ############### !! BE CAREFUL !! ############### */
    /* ################################################ */
  }
}
sealed trait XmlNodeSyntax {

  import cats.syntax.all.*

  implicit class GenericXmlNodeReadOps[K <: XmlNode](genericNode: K) {

    // ------------------ ATTRS ------------------
    def findAttrRaw(key: String): Option[XmlAttribute] =
      genericNode.attributes.find(_.key == key)

    def findAttr(key: String): Option[String] =
      genericNode.attributes
        .find(_.key == key)
        .map(_.value.asString)

    def findAttr[T: Decoder](key: String): Option[T] =
      genericNode.attributes
        .find(_.key == key)
        .flatMap(_.value.as[T].toOption)

    def findAttrWhere[T: Decoder](keyP: String => Boolean, valueP: T => Boolean): Option[T] =
      genericNode.attributes
        .mapFilter(a => {
          if (keyP(a.key))
            a.value.as[T].toOption.filter(valueP)
          else
            None
        })
        .headOption

    def existsAttrByKey(p: String => Boolean): Boolean =
      genericNode.attributes.exists(a => p(a.key))

    def existsAttrWithValue[T: Decoder](key: String, valueP: T => Boolean): Boolean =
      existsAttrByKeyAndValue(_.eqv(key), valueP)

    def existsAttrByKeyAndValue[T: Decoder](
      keyP: String => Boolean,
      valueP: T => Boolean
    ): Boolean =
      findAttrWhere(keyP, valueP).isDefined

    // ------------------ CHILDREN ------------------
    // find
    def findChild(thatLabel: String): Option[XmlNode.Node] =
      findChildBy(_.label == thatLabel)
        .asInstanceOf[Option[XmlNode.Node]]

    def findChildBy(p: XmlNode => Boolean): Option[XmlNode] =
      genericNode.children.find(p)

    def findDeepChild(thatLabel: String): Option[XmlNode.Node] =
      findDeepChildBy(_.label == thatLabel)
        .asInstanceOf[Option[XmlNode.Node]]

    def findDeepChildBy(p: XmlNode => Boolean): Option[XmlNode] =
      deepSubNodes.find(p)

    // filter
    def filterChildren(thatLabel: String): List[XmlNode.Node] =
      filterChildrenBy(_.label == thatLabel)
        .asInstanceOf[List[XmlNode.Node]]

    def filterChildrenBy(p: XmlNode => Boolean): List[XmlNode] =
      genericNode.children.filter(p).toList

    def filterDeepChildren(thatLabel: String): LazyList[XmlNode.Node] =
      filterDeepChildrenBy(_.label == thatLabel)
        .asInstanceOf[LazyList[XmlNode.Node]]

    def filterDeepChildrenBy(p: XmlNode => Boolean): LazyList[XmlNode] =
      deepSubNodes.filter(p)

    def deepSubNodes: LazyList[XmlNode] = {

      @tailrec
      def rec(left: List[XmlNode], acc: LazyList[XmlNode]): LazyList[XmlNode] =
        left match {
          case Nil          => acc
          case head :: tail => rec(tail, acc.appended(head).lazyAppendedAll(head.deepSubNodes))
        }

      genericNode.content.children match {
        case Nil                 => LazyList.empty
        case currentNodeChildren => rec(currentNodeChildren, LazyList.empty)
      }
    }
  }

  implicit class GenericXmlNodeWriteOps[K <: XmlNode](genericNode: K) {

    def drainContent: K#Self =
      withContent(NodeContent.empty)

    def withContent(newContent: NodeContent): K#Self =
      genericNode.updateContent(_ => newContent)

    def withOptContent(newContent: Option[NodeContent]): K#Self =
      genericNode.updateContent(_ => newContent.getOrElse(NodeContent.empty))

    // ------------------ CHILDREN ------------------
    def withChild(child: XmlNode, children: XmlNode*): K#Self =
      withChildren(child +: children)

    def withChildren(children: Seq[XmlNode]): K#Self =
      withContent(NodeContent.childrenSeq(children).getOrElse(NodeContent.empty))

    def appendChild(child: XmlNode, children: XmlNode*): K#Self =
      updateChildren(currentChildren => currentChildren ++ List(child) ++ children)

    def prependChild(child: XmlNode, children: XmlNode*): K#Self =
      updateChildren(currentChildren => List(child) ++ children ++ currentChildren)

    def updateChildren(f: Endo[Seq[XmlNode]]): K#Self =
      genericNode.updateContent(currentContent =>
        NonEmptyList.fromFoldable(f(currentContent.children)) match {
          case Some(newChildrenNel) => NodeContent.children(newChildrenNel)
          case None                 => NodeContent.empty
        }
      )
  }

  implicit class XmlNodeNodeWriteOps(node: XmlNode.Node) {

    // ------------------ LABEL ------------------
    /** Rename node label. This method isn't pure for usability purpose.
      *
      * @param newLabel
      *   new label value
      * @return
      *   Same node with updated label
      * @throws IllegalArgumentException
      *   If the new label values is not valid. See [[Xml.isValidXmlName]]
      */
    @impure
    def withLabel(newLabel: String): XmlNode.Node =
      node.updateLabel(_ => newLabel)

    // ------------------ ATTRS ------------------
    def withAttributes(attrs: Seq[XmlAttribute]): XmlNode.Node =
      node.updateAttrs(_ => attrs.toList)

    def withAttributes(attr: XmlAttribute, attrs: XmlAttribute*): XmlNode.Node =
      node.updateAttrs(_ => (attr +: attrs).toList)

    // append attrs
    def appendAttr(newAttr: XmlAttribute): XmlNode.Node =
      node.updateAttrs(ls => ls :+ newAttr)

    def appendAttrs(newAttr: XmlAttribute, newAttrs: XmlAttribute*): XmlNode.Node =
      node.appendAttrs(newAttr +: newAttrs)

    def appendAttrs(newAttrs: Seq[XmlAttribute]): XmlNode.Node =
      node.updateAttrs(ls => ls ++ newAttrs)

    // prepend attrs
    def prependAttr(newAttr: XmlAttribute): XmlNode.Node =
      node.updateAttrs(ls => newAttr +: ls)

    def prependAttrs(newAttr: XmlAttribute, newAttrs: XmlAttribute*): XmlNode.Node =
      node.prependAttrs(newAttr +: newAttrs)

    def prependAttrs(newAttrs: Seq[XmlAttribute]): XmlNode.Node =
      node.updateAttrs(ls => (newAttrs ++ ls).toList)

    def removeAttr(key: String): XmlNode.Node =
      node.updateAttrs(_.filterNot(_.key == key))

    def updateAttr(key: String)(f: Endo[XmlAttribute]): XmlNode.Node =
      node.updateAttrs(_.map(attr => if (attr.key == key) f(attr) else attr))

    // ------------------ CONTENT - TEXT ------------------
    /** Set node content to Text with the specified data. All children nodes will be removed.
      */
    def withText[T: DataEncoder](data: T): XmlNode.Node =
      node.withContent(NodeContent.textOrEmpty(data))

    /** Decode and then update node text if content is text.
      *
      * If you need raw data see [[updateTextRaw]]
      */
    def updateText[T: Decoder: DataEncoder](f: Endo[T])(implicit dmi: DummyImplicit): XmlNode.Node =
      updateText[T, T](f)

    /** Decode and then update node text if content is text.
      *
      * If you need raw data see [[updateTextRaw]]
      */
    def updateText[T1: Decoder, T2: DataEncoder](f: T1 => T2): XmlNode.Node =
      node.text.flatMap(Decoder[T1].decode(_).toOption.map(f)) match {
        case Some(data) => withText(data)
        case None       => node
      }

    /** Update node text if content is text.
      *
      * If you need decoded data see [[updateText]]
      */
    def updateTextRaw[T: DataEncoder](f: XmlData => T): XmlNode.Node =
      node.text.map(f) match {
        case Some(data) => withText(data)
        case None       => node
      }
  }
}
sealed trait XmlNodeInstances {

  implicit val monoidXmlNodeGroup: Monoid[XmlNode] = new Monoid[XmlNode] {
    override def empty: XmlNode = XmlNode.emptyGroup

    override def combine(x: XmlNode, y: XmlNode): XmlNode =
      (x, y) match {
        case (x1: XmlNode.Group, x2: XmlNode.Group) =>
          XmlNode.group(x1.children ++ x2.children)
        case (x1: XmlNode.Node, x2: XmlNode.Group) =>
          XmlNode.group(x1 +: x2.children)
        case (x1: XmlNode.Group, x2: XmlNode.Node) =>
          XmlNode.group(x1.children :+ x2)
        case (x1: XmlNode.Node, x2: XmlNode.Node) =>
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
