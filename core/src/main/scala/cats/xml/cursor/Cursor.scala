package cats.xml.cursor

import cats.xml.{Xml, XmlNode}
import cats.xml.codec.Decoder
import cats.Show

import scala.annotation.unused

sealed trait Cursor[+X <: Xml] extends Serializable {

  def path: String

  def focus(input: XmlNode): Cursor.Result[X]

  def as[T: Decoder]: FreeCursor[Xml, T] =
    FreeCursor[T](this)

  override final def toString: String = path
}

object Cursor {

  import cats.syntax.all.*

  type Result[+T] = Either[CursorFailure, T]

  def failed[X <: Xml](failure: CursorFailure): Cursor[X] =
    Cursor.const(Left(failure))

  def const[X <: Xml](result: Cursor.Result[X]): Cursor[X] = new Cursor[X] {
    override def path: String                                    = ""
    override def focus(@unused input: XmlNode): Cursor.Result[X] = result
  }

  private[cursor] trait CursorOp {
    override final def toString: String = Show[CursorOp].show(this)
  }
  private[cursor] object CursorOp {

    def buildOpsPath(ops: => List[CursorOp]): String =
      ops.map(_.show).mkString("")

    implicit val show: Show[CursorOp] = Show.show {
      case op: AttrCursor.Op => op.show
      case op: NodeCursor.Op => op.show
      case _                 => "unsupported"
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
