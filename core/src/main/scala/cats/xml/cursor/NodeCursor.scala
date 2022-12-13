package cats.xml.cursor

import cats.{Endo, Eq, Show}
import cats.xml.XmlNode
import cats.xml.cursor.Cursor.{CursorOp, Result}
import cats.xml.modifier.{Modifier, ModifierFailure}

import scala.annotation.{tailrec, unused}
import scala.collection.mutable.ListBuffer

/** Vertical cursor for nodes
  */
sealed trait NodeCursor extends Dynamic with VCursor[XmlNode, NodeCursor] { $this =>

  def history: List[NodeCursor.Op]

  val isRoot: Boolean = false

  override final lazy val path: String = CursorOp.buildOpsPath(history)

  def andThen(nextNodeCursor: NodeCursor): NodeCursor =
    new NodeCursor {
      override def history: List[NodeCursor.Op] =
        $this.history ++ nextNodeCursor.history

      override protected val lastCursor: NodeCursor =
        nextNodeCursor.lastCursor

      override def focus(input: XmlNode): Result[XmlNode] =
        $this.focus(input).flatMap(node => nextNodeCursor.focus(node))
    }

  def modifyIfNode(modifier: Endo[XmlNode.Node]): Modifier[XmlNode] =
    modify {
      case node: XmlNode.Node   => modifier(node)
      case group: XmlNode.Group => group
    }

  def modify(modifier: Endo[XmlNode]): Modifier[XmlNode] =
    Modifier(node => {
      val nodeClone = node.duplicate
      focus(nodeClone) match {
        case Right(focus) =>
          focus.unsafeMute(modifier)
          Right(focus)
        case Left(failure) =>
          Left(ModifierFailure.CursorFailed(failure))
      }
    })

  // node
  def noop: NodeCursor = this

  def * : NodeCursor =
    downWildcard

  def downWildcard: NodeCursor =
    move(NodeCursor.Op.DownWildcard)

  def selectDynamic(nodeName: String): NodeCursor =
    down(nodeName)

  def \(nodeName: String): NodeCursor =
    down(nodeName)

  def \\(nodeName: String): NodeCursor =
    deepDown(nodeName)

  def downPath(path: String): NodeCursor =
    path.split("/").foldLeft(this)(_.down(_))

  def down(nodeName: String): NodeCursor =
    move(NodeCursor.Op.Down(nodeName))

  def deepDown(nodeName: String): NodeCursor =
    move(NodeCursor.Op.DeepDown(nodeName))

  def applyDynamic(nodeName: String)(index: Int): NodeCursor =
    down(nodeName)(index)

  def apply(index: Int): NodeCursor =
    move(NodeCursor.Op.SelectNodeByIndex(index))

  def |(p: XmlNode => Boolean): NodeCursor =
    filter(p)

  def filter(p: XmlNode => Boolean): NodeCursor =
    move(NodeCursor.Op.Filter(p))

  def head: NodeCursor =
    move(NodeCursor.Op.Head)

  def last: NodeCursor =
    move(NodeCursor.Op.Last)

  def atIndex(index: Int): NodeCursor =
    move(NodeCursor.Op.SelectNodeByIndex(index))

  def find(p: XmlNode => Boolean): NodeCursor =
    move(NodeCursor.Op.FindChild(p))

  private def move(op: NodeCursor.Op) =
    new NodeCursor.Simple(this, op)

  // content
  def attr(key: String): AttrCursor =
    new AttrCursor(this, AttrCursor.Op.SelectAttr(key))

  def attrAt(index: Long): AttrCursor =
    new AttrCursor(this, AttrCursor.Op.SelectAttrByIndex(index))

  def attrHead: AttrCursor =
    new AttrCursor(this, AttrCursor.Op.Head)

  def attrLast: AttrCursor =
    new AttrCursor(this, AttrCursor.Op.Last)

  def text: TextCursor =
    new TextCursor(this)

  // eq
  override final def equals(obj: Any): Boolean =
    obj.isInstanceOf[NodeCursor]
      && Eq[NodeCursor].eqv(this, obj.asInstanceOf[NodeCursor])
}

object NodeCursor {

  def endo(f: NodeCursor => NodeCursor): Endo[NodeCursor] = f

  def failed(failure: CursorFailure): NodeCursor =
    NodeCursor.const(Left(failure))

  def const(result: Cursor.Result[XmlNode]): NodeCursor = new NodeCursor {
    override def history: List[Op]                              = Nil
    override protected val lastCursor: NodeCursor               = this
    override val isRoot: Boolean                                = false
    override def focus(@unused input: XmlNode): Result[XmlNode] = result
  }

  sealed trait Op extends CursorOp
  object Op {
    case class Down(nodeName: String) extends Op
    case class DeepDown(nodeName: String) extends Op
    case object DownWildcard extends Op
    case class SelectNodeByIndex(index: Int) extends Op
    case class Filter(p: XmlNode => Boolean) extends Op
    case class FindChild(p: XmlNode => Boolean) extends Op
    case object Head extends Op
    case object Last extends Op

    implicit final val show: Show[Op] = Show.show {
      case Down(nodeName)           => s"/$nodeName"
      case DeepDown(nodeName)       => s"//$nodeName"
      case DownWildcard             => s"/*"
      case SelectNodeByIndex(index) => s"[$index]"
      case Filter(p)                => s"[filter $p]"
      case FindChild(p)             => s"[find $p]"
      case Head                     => s"/head"
      case Last                     => s"/last"
    }
  }

  // kind
  case object Root extends NodeCursor {

    override protected val lastCursor: NodeCursor = this

    override val isRoot: Boolean = true

    override def history: List[Op] = Nil

    override def focus(xml: XmlNode): Cursor.Result[XmlNode] = Right(xml)
  }

  final class Simple(protected val lastCursor: NodeCursor, protected val lastOp: Op)
      extends NodeCursor {

    import cats.implicits.*

    override def focus(ns: XmlNode): Cursor.Result[XmlNode] = {
      @tailrec
      def rec(
        history: List[NodeCursor.Op],
        currentPath: List[NodeCursor.Op],
        current: XmlNode
      ): Cursor.Result[XmlNode] = {
        history match {
          case Nil => Right(current)
          case op :: ops =>
            val result: Cursor.Result[XmlNode] = op match {
              case NodeCursor.Op.Down(nodeName) =>
                current
                  .fold(
                    ifNode = _.filterChildren(nodeName).asRight,
                    ifGroup = group =>
                      if (group.isEmpty)
                        CursorFailure
                          .MissingNodeInEmptyGroup(CursorOp.buildOpsPath(currentPath), nodeName)
                          .asLeft
                      else
                        group.children.flatMap(_.filterChildren(nodeName)).asRight
                  )
                  .flatMap {
                    case Nil =>
                      CursorFailure
                        .MissingNode(CursorOp.buildOpsPath(currentPath), nodeName)
                        .asLeft
                    case results =>
                      XmlNode.fromSeq(results).asRight
                  }

              case NodeCursor.Op.DeepDown(nodeName) =>
                XmlNode
                  .fromSeq(
                    current
                      .filterDeepChildren(nodeName)
                      .toList
                  )
                  .asRight[CursorFailure]
              case NodeCursor.Op.DownWildcard =>
                Right(XmlNode.fromSeq(current.children))
              case NodeCursor.Op.SelectNodeByIndex(index) =>
                current.children
                  .lift(index)
                  .toRight(
                    CursorFailure.MissingNodeAtIndex(CursorOp.buildOpsPath(currentPath), index)
                  )
              case NodeCursor.Op.Filter(predicate) =>
                current
                  .fold(
                    ifNode = predicate(_) match {
                      case true  => XmlNode.group(current).asRight[CursorFailure]
                      case false => XmlNode.emptyGroup.asRight[CursorFailure]
                    },
                    ifGroup = group =>
                      XmlNode.fromSeq(group.filterChildrenBy(predicate)).asRight[CursorFailure]
                  )

              case NodeCursor.Op.FindChild(p) =>
                current.children
                  .find(p)
                  .toRight(
                    CursorFailure.MissingNodeFind(CursorOp.buildOpsPath(currentPath), p)
                  )
              case NodeCursor.Op.Head =>
                current.children.headOption
                  .toRight(
                    CursorFailure.MissingNodeHead(CursorOp.buildOpsPath(currentPath))
                  )
              case NodeCursor.Op.Last =>
                current.children.lastOption
                  .toRight(
                    CursorFailure.MissingNodeLast(CursorOp.buildOpsPath(currentPath))
                  )
            }

            result match {
              case Right(node)                  => rec(ops, currentPath :+ op, node)
              case Left(failure: CursorFailure) => Left(failure)
            }
        }
      }

      rec(
        history     = this.history,
        currentPath = Nil,
        current     = ns
      )
    }

    def history: List[Op] = {
      var next: NodeCursor = this
      val builder          = new ListBuffer[Op]

      while (!next.isRoot) {
        val n = next.asInstanceOf[Simple]
        n.lastOp +=: builder
        next = n.lastCursor
      }

      builder.result()
    }

  }

  // instances
  implicit val eq: Eq[NodeCursor] =
    (x: NodeCursor, y: NodeCursor) => (x.isRoot && y.isRoot) || (x.path == y.path)

  implicit val show: Show[NodeCursor] = Show.fromToString
}
