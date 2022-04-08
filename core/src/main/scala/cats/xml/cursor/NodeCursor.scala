package cats.xml.cursor

import cats.{Endo, Show}
import cats.xml.XmlNode
import cats.xml.cursor.Cursor.CursorOp
import cats.xml.modifier.{Modifier, ModifierFailure}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/** Vertical cursor for nodes
  */
sealed trait NodeCursor extends Dynamic with VCursor[XmlNode, NodeCursor] {

  def history: List[NodeCursor.Op]

  override lazy val path: String = CursorOp.buildOpsPath(history)

  def modify(modifier: Endo[XmlNode]): Modifier[XmlNode] =
    Modifier(node => {
      val nodeClone = node.copy()
      focus(nodeClone) match {
        case Right(focus) =>
          focus.mute(modifier)
          Right(focus)
        case Left(failure) =>
          Left(ModifierFailure.CursorFailed(failure))
      }
    })

  // node
  def selectDynamic(nodeName: String): NodeCursor =
    down(nodeName)

  def \(nodeName: String): NodeCursor =
    down(nodeName)

  def downPath(path: String): NodeCursor =
    path.split("\\\\").foldLeft(this)(_.down(_))

  def down(nodeName: String): NodeCursor =
    new NodeCursor.Simple(this, NodeCursor.Op.Down(nodeName))

  // content
  def attr(key: String): AttrCursor =
    new AttrCursor(this, AttrCursor.Op.SelectAttr(key))

  def attrHead: AttrCursor =
    new AttrCursor(this, AttrCursor.Op.Head)

  def attrLast: AttrCursor =
    new AttrCursor(this, AttrCursor.Op.Last)

  def text: TextCursor =
    new TextCursor(this)
}

object NodeCursor {

  sealed trait Op extends CursorOp
  object Op {

    case class Down(nodeName: String) extends Op

    implicit final val showCursorOp: Show[Op] = Show.show { case Down(nodeName) =>
      s"/$nodeName"
    }
  }

  // kind
  case object Root extends NodeCursor {

    override protected val lastCursor: NodeCursor = this

    override def history: List[Op] = Nil

    override def focus(xml: XmlNode): Cursor.Result[XmlNode] = Right(xml)
  }

  class Simple(protected val lastCursor: NodeCursor, protected val lastOp: Op) extends NodeCursor {

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
                  .findChild(nodeName)
                  .toRight(
                    CursorFailure.MissingNode(nodeName, CursorOp.buildOpsPath(currentPath))
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

      while (!next.equals(Root)) {
        val n = next.asInstanceOf[Simple]
        n.lastOp +=: builder
        next = n.lastCursor
      }

      builder.result()
    }
  }
}
