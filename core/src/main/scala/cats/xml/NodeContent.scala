package cats.xml

import cats.data.NonEmptyList
import cats.kernel.Eq
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

  final def duplicate: NodeContent =
    this match {
      case NodeContent.Empty =>
        NodeContent.Empty
      case NodeContent.Text(data) =>
        NodeContent.Text(data)
      case NodeContent.Children(children: NonEmptyList[XmlNode]) =>
        NodeContent.Children(children.map(_.duplicate))
    }
}
object NodeContent extends NodeContentInstances {

  final val empty: NodeContent = Empty

  def text[T: DataEncoder](data: T): NodeContent = {
    val encData = DataEncoder[T].encode(data)
    if (encData.isEmpty) NodeContent.empty else Text(encData)
  }

  def children(node: XmlNode, nodes: XmlNode*): NodeContent =
    children(node +: nodes)

  def childrenNel(childrenNel: NonEmptyList[XmlNode]): NodeContent =
    children(childrenNel.toList)

  def children(childrenLs: Seq[XmlNode]): NodeContent =
    NonEmptyList
      .fromList(
        childrenLs
          .flatMap(
            _.fold(
              ifNode  = Seq(_),
              ifGroup = _.children
            ).filterNot(_.isNull)
          )
          .toList
      )
      .fold(NodeContent.empty)(Children(_))

  case object Empty extends NodeContent
  final case class Text(data: XmlData) extends NodeContent
  object Text {
    private[NodeContent] def apply(data: XmlData): Text = new Text(data)
  }
  final case class Children(childrenNel: NonEmptyList[XmlNode]) extends NodeContent
  object Children {
    private[NodeContent] def apply(childrenNel: NonEmptyList[XmlNode]): Children = new Children(
      childrenNel
    )
  }
}
sealed trait NodeContentInstances {

  import cats.implicits.catsSyntaxEq

  implicit val eqNodeContent: Eq[NodeContent] = Eq.instance {
    case (NodeContent.Empty, NodeContent.Empty)             => true
    case (NodeContent.Text(data1), NodeContent.Text(data2)) => data1 === data2
    case (NodeContent.Children(children1), NodeContent.Children(children2)) =>
      children1 === children2
    case _ => false
  }
}
