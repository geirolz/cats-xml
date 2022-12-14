package cats.xml.cursor

import cats.{Endo, Show}
import cats.xml.{Xml, XmlNode}
import cats.xml.codec.{DataEncoder, Decoder}
import cats.xml.modifier.Modifier

import scala.annotation.unused

sealed trait Cursor[+X <: Xml] extends Serializable {

  type Focus

  /** Apply the current cursor to the specified input. This allows to select a precise part of the
    * `Xml` tree.
    *
    * The method is pure and return a Left when the focusing fails
    * @param input
    *   target of the cursor
    * @return
    *   `Right` when succeed `Left` when fail
    */
  def focus(input: XmlNode): Cursor.Result[X]

  /** Create a `FreeCursor` which represent a cursor with a free `T` type as result of the focusing,
    * this implies that a `Decoder` instance is need to convert the fixed `Xml` type of the `Cursor`
    * to the free `T` type of the `FreeCursor`.
    *
    * So, practically speaking, when the `FreeCursor` is applied this `Cursor` is applied and the
    * result is decoded using the specified `Decoder` for type `T`.
    *
    * @tparam T
    *   free type in which decode the current cursor result
    * @return
    *   A new `FreeCursor`
    */
  def as[T: Decoder]: FreeCursor[Xml, T] =
    FreeCursor[T](this)

  /** A String representation of the cursor.
    *
    * @return
    *   a String which represent the cursor path
    */
  def path: String

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

  override type Focus = HFocus

  protected val lastCursor: VC

  final def up: VC = lastCursor
}

trait HCursor[HFocus <: Xml, +VC <: VCursor[?, VC], +HC <: HCursor[?, ?, HC]]
    extends Cursor[HFocus] {

  override type Focus = HFocus

  protected val vCursor: VC

  final def up: VC = vCursor

  def head: HC

  def last: HC

  def left: HC

  def right: HC
}

private[cursor] trait WithModifierSupport[X <: Xml] { this: Cursor[X] =>
  def modify(modifier: Endo[X]): Modifier[XmlNode]
}
private[cursor] trait WithDataModifierSupport[X <: Xml] extends WithModifierSupport[X] {
  this: Cursor[X] =>

  def set[U: DataEncoder](newValue: U): Modifier[XmlNode] =
    modify[U](_ => newValue)

  def modify[U: DataEncoder](f: String => U): Modifier[XmlNode] =
    modify[String, U](f)

  def modify[T: Decoder, U: DataEncoder](f: T => U): Modifier[XmlNode]
}
