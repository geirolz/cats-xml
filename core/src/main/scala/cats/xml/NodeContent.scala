package cats.xml

import cats.data.NonEmptyList
import cats.xml.codec.DataEncoder

/** Coproduct to define XML node content
  */
sealed trait NodeContent {

  final lazy val isEmpty: Boolean = this match {
    case NodeContent.Empty => true
    case _                 => false
  }

  final lazy val isText: Boolean = text.isDefined

  final lazy val nonEmpty: Boolean = !isEmpty

  final lazy val isChildren: Boolean = this match {
    case NodeContent.Children(_) => true
    case _                       => false
  }

  final lazy val text: Option[XmlData] =
    this match {
      case NodeContent.Text(data) => Some(data)
      case _                      => None
    }

  final lazy val children: List[XmlNode] = this match {
    case NodeContent.Children(children) => children.toList
    case _                              => Nil
  }
}
object NodeContent {

  final val empty: NodeContent = Empty

  def text[T: DataEncoder](data: T): Option[NodeContent] = {
    val encData = DataEncoder[T].encode(data)
    if (encData.isEmpty) None else Some(Text(encData))
  }

  def textOrEmpty[T: DataEncoder](data: T): NodeContent =
    text[T](data).getOrElse(NodeContent.empty)

  def childrenSeq(childrenLs: Seq[XmlNode]): Option[NodeContent] =
    NonEmptyList.fromList(childrenLs.toList).map(children)

  def children(node: XmlNode, nodes: XmlNode*): NodeContent =
    Children(NonEmptyList.of(node, nodes*))

  def children(childrenNel: NonEmptyList[XmlNode]): NodeContent =
    Children(childrenNel)

  def childrenOrEmpty(childrenLs: Seq[XmlNode]): NodeContent =
    childrenSeq(childrenLs).getOrElse(NodeContent.empty)

  case object Empty extends NodeContent
  final case class Text private[NodeContent] (data: XmlData) extends NodeContent
  final case class Children private[NodeContent] (childrenNel: NonEmptyList[XmlNode])
      extends NodeContent
}
