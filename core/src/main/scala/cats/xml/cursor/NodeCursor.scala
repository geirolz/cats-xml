package cats.xml.cursor

import cats.{Endo, Show}
import cats.xml.XmlNode
import cats.xml.cursor.Cursor.CursorOp
import cats.xml.cursor.CursorResult.{Failed, Focused}
import cats.xml.modifier.Modifier

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/** Vertical cursor for nodes
  */
sealed trait NodeCursor extends Dynamic with VCursor[XmlNode, NodeCursor] {

  def history: List[NodeCursor.Op]

  override lazy val path: String = CursorOp.buildOpsPath(history)

  def modify(modifier: Endo[XmlNode]): Modifier[XmlNode] =
    Modifier.fromNodeCursor(this, modifier)

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

    override def focus(xml: XmlNode): CursorResult[XmlNode] =
      CursorResult.Focused(xml)
  }

  class Simple(protected val lastCursor: NodeCursor, protected val lastOp: Op) extends NodeCursor {

    override def focus(ns: XmlNode): CursorResult[XmlNode] = {
      @tailrec
      def rec(
        history: List[NodeCursor.Op],
        currentPath: List[NodeCursor.Op],
        current: XmlNode
      ): CursorResult[XmlNode] = {
        history match {
          case Nil =>
            CursorResult.Focused(current)
          case op :: ops =>
            val result: CursorResult[XmlNode] = op match {
              case NodeCursor.Op.Down(nodeName) =>
                current.findChild(nodeName) match {
                  case Some(node) =>
                    CursorResult.Focused(node)
                  case None =>
                    CursorResult.MissingNode(nodeName, CursorOp.buildOpsPath(currentPath))
                }
            }

            result match {
              case Focused(node)  => rec(ops, currentPath :+ op, node)
              case failed: Failed => failed
            }
        }
      }

      rec(this.history, Nil, ns)
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
