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

  final lazy val nonEmpty: Boolean = !isEmpty

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

  def text[T: DataEncoder](data: T): NodeContent =
    Text(DataEncoder[T].encode(data))

  def childrenSeq(childrenLs: Seq[XmlNode]): Option[NodeContent] =
    NonEmptyList.fromList(childrenLs.toList).map(children)

  def children(node: XmlNode, nodes: XmlNode*): NodeContent =
    Children(NonEmptyList.of(node, nodes*))

  def children(childrenNel: NonEmptyList[XmlNode]): NodeContent =
    Children(childrenNel)

  def childrenOrEmpty(childrenLs: Seq[XmlNode]): NodeContent =
    childrenSeq(childrenLs).getOrElse(NodeContent.empty)

  case object Empty extends NodeContent
  case class Text(data: XmlData) extends NodeContent
  case class Children(childrenNel: NonEmptyList[XmlNode]) extends NodeContent
}
