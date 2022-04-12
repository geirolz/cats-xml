package cats.xml.cursor

import cats.xml.{Xml, XmlNode}
import cats.xml.codec.Decoder
import cats.Show

sealed trait Cursor[+X <: Xml] extends Serializable {

  def path: String

  def focus(input: XmlNode): Cursor.Result[X]

  def as[T: Decoder]: FreeCursor[Xml, T] =
    FreeCursor[T](this)
}

object Cursor {

  type Result[+T] = Either[CursorFailure, T]

  trait CursorOp {
    override def toString: String = Show[CursorOp].show(this)
  }
  object CursorOp {

    implicit val showInstanceForCursorOp: Show[CursorOp] = Show.show {
      case op: AttrCursor.Op => Show[AttrCursor.Op].show(op)
      case op: NodeCursor.Op => Show[NodeCursor.Op].show(op)
      case op                => op.toString
    }

    def buildOpsPath(ops: List[CursorOp]): String = {
      ops.map(Show[CursorOp].show(_)).mkString("")
    }
  }
}

trait VCursor[HFocus <: Xml, +VC <: VCursor[?, VC]] extends Dynamic with Cursor[HFocus] {

  type Focus = HFocus

  protected val lastCursor: VC

  final def up: VC = lastCursor
}

trait HCursor[HFocus <: Xml, +VC <: VCursor[?, VC], +HC <: HCursor[?, ?, HC]]
    extends Cursor[HFocus] {

  type Focus = HFocus

  protected val vCursor: VC

  final def up: VC = vCursor

  def head: HC

  def last: HC

  def left: HC

  def right: HC
}
