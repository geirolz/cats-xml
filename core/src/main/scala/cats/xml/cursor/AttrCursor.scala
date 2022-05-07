package cats.xml.cursor

import cats.Show
import cats.xml.{XmlAttribute, XmlData, XmlNode}
import cats.xml.codec.DataEncoder
import cats.xml.cursor.AttrCursor.Op
import cats.xml.cursor.Cursor.CursorOp
import cats.xml.modifier.{Modifier, ModifierFailure}

/** Horizontal cursor for node attributes
  */
class AttrCursor(protected val vCursor: NodeCursor, op: AttrCursor.Op)
    extends HCursor[XmlAttribute, NodeCursor, AttrCursor] { $this =>

  import cats.implicits.*

  final lazy val path: String = s"${vCursor.path}$op"

  // modify
  def modify[T: DataEncoder](f: XmlData => T): Modifier[XmlNode] =
    Modifier(node =>
      $this.focus(node) match {
        case Right(attr) =>
          vCursor.modify(_.updateAttr(attr.key)(_ => attr.map(f)))(node)
        case Left(failure) =>
          ModifierFailure.CursorFailed(failure).asLeft
      }
    )

  // focus
  override def focus(xml: XmlNode): Cursor.Result[XmlAttribute] = {

    def applyFocus(node: XmlNode, op: Op): Cursor.Result[XmlAttribute] =
      op match {
        case Op.SelectAttr(key) =>
          node
            .findAttr(key)
            .toRight(CursorFailure.MissingAttrByKey(path, key))

        case Op.SelectAttrByIndex(index) =>
          node.attributes
            .get(index)
            .toRight(CursorFailure.MissingAttrAtIndex(path, index))

        case Op.Head =>
          node.attributes.headOption
            .toRight(CursorFailure.MissingAttrHead(path))

        case Op.Last =>
          node.attributes.lastOption
            .toRight(CursorFailure.MissingAttrLast(path))

        // nested
        case Op.Left(currentOp) =>
          applyFocus(node, currentOp)
            .flatMap(attr =>
              node.children.headOption
                .flatMap(
                  _.attributes
                    .takeWhile(_.key.ne(attr.key))
                    .lastOption
                )
                .toRight(CursorFailure.LeftBoundLimitAttr(path, attr.key))
            )

        case Op.Right(currentOp) =>
          applyFocus(node, currentOp)
            .flatMap(attr =>
              node.children.headOption
                .flatMap(n => {
                  val attrs = n.attributes
                  val prev  = attrs.takeWhile(_.key.ne(attr.key))
                  val next  = attrs.drop(prev.size + 1)

                  next.headOption
                })
                .toRight(CursorFailure.RightBoundLimitAttr(path, attr.key))
            )
      }

    vCursor.focus(xml).flatMap(applyFocus(_, op))
  }

  override def head: AttrCursor =
    move(AttrCursor.Op.Head)

  override def last: AttrCursor =
    move(AttrCursor.Op.Last)

  override def left: AttrCursor =
    move(AttrCursor.Op.Left(op))

  override def right: AttrCursor =
    move(AttrCursor.Op.Right(op))

  private def move(op: AttrCursor.Op): AttrCursor =
    new AttrCursor(vCursor, op)
}
object AttrCursor {

  sealed trait Op extends CursorOp
  object Op {
    case class SelectAttr(key: String) extends Op
    case class SelectAttrByIndex(index: Long) extends Op
    case object Head extends Op
    case object Last extends Op
    case class Left(currentOp: Op) extends Op
    case class Right(currentOp: Op) extends Op

    implicit final val showCursorOp: Show[Op] = Show.show {
      case SelectAttr(key)          => s"/@$key"
      case SelectAttrByIndex(index) => s"/@[$index]"
      case Head                     => "/@[0]"
      case Last                     => "/@[last]"
      case Left(op)                 => s"${showCursorOp.show(op)}/<-@"
      case Right(op)                => s"${showCursorOp.show(op)}/@->"
    }
  }
}
