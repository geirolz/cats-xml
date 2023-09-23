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
}
object NodeContent extends NodeContentInstances {

  final val empty: NodeContent = Empty

  def text[T: DataEncoder](data: T): Option[NodeContent] = {
    val encData = DataEncoder[T].encode(data)
    if (encData.isEmpty) None else Some(Text(encData))
  }

  def textOrEmpty[T: DataEncoder](data: T): NodeContent =
    text[T](data).getOrElse(NodeContent.empty)

  def children(childrenLs: Seq[XmlNode]): Option[NodeContent] =
    NonEmptyList.fromList(childrenLs.toList.filterNot(_.isNull)).map(Children(_))

  def children(node: XmlNode, nodes: XmlNode*): NodeContent =
    Children(NonEmptyList.of(node, nodes*))

  def childrenNel(childrenNel: NonEmptyList[XmlNode]): NodeContent =
    childrenOrEmpty(childrenNel.toList)

  def childrenOrEmpty(childrenLs: Seq[XmlNode]): NodeContent =
    children(childrenLs).getOrElse(NodeContent.empty)

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
