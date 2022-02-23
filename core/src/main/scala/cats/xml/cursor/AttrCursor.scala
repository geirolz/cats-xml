package cats.xml.cursor

import cats.xml.{XmlAttribute, XmlNode}
import cats.xml.cursor.AttrCursor.Op
import cats.xml.cursor.Cursor.CursorOp
import cats.xml.cursor.CursorResult.*
import cats.Show

/** Horizontal cursor for node attributes
  */
class AttrCursor(protected val vCursor: NodeCursor, op: AttrCursor.Op)
    extends HCursor[XmlAttribute, NodeCursor, AttrCursor] {

  import cats.implicits.*
  lazy val path: String = s"${vCursor.path}${op.show}"

  // modifier
//  def rename(newK: String): Modifier[Attribute] =
//    if (newK.isEmpty)
//      Modifier.fail(this, ModifierResult.InvalidData("", newK))
//    else
//      modify(_.copy(key = newK))

//  def set(v: => String): Modifier[Attribute] =
//    modify(_.copy(value = v))
//
//  def update(f: Endo[String]): Modifier[Attribute] =
//    modify(a => a.copy(value = f(a.value)))

  // focus
//  private[xml]
  override def focus(xml: XmlNode): CursorResult[XmlAttribute] = {

    // TODO: tail rec
    def applyFocus(node: XmlNode, op: Op): CursorResult[XmlAttribute] =
      op match {
        case Op.SelectAttr(key) =>
          CursorResult.fromOption(
            node.findAttr(key)
          )(ifEmpty = MissingAttrByKey(path, key))

        case Op.SelectAttrByIndex(index) =>
          CursorResult.fromOption(
            node.attributes.get(index)
          )(ifEmpty = MissingAttrAtIndex(path, index))

        case Op.Head =>
          CursorResult.fromOption(
            node.attributes.headOption
          )(ifEmpty = MissingAttrHead(path))

        case Op.Last =>
          CursorResult.fromOption(
            node.attributes.lastOption
          )(ifEmpty = MissingAttrLast(path))

        // nested
        case Op.Left(leftOp) =>
          applyFocus(node, leftOp)
            .flatMap(attr =>
              CursorResult.fromOption(
                node.children.headOption
                  .flatMap(
                    _.attributes
                      .takeWhile(_.key.ne(attr.key))
                      .lastOption
                  )
              )(LeftBoundLimitAttr(path, attr.key))
            )
        case Op.Right(rightOp) =>
          applyFocus(node, rightOp)
            .flatMap(attr =>
              CursorResult.fromOption(
                node.children.headOption
                  .flatMap(n => {
                    val attrs = n.attributes
                    val prev  = attrs.takeWhile(_.key.ne(attr.key))
                    val next  = attrs.drop(prev.size + 1)

                    next.headOption
                  })
              )(RightBoundLimitAttr(path, attr.key))
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
    case class Left(op: Op) extends Op
    case class Right(op: Op) extends Op

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
