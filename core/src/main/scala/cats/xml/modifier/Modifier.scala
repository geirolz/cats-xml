package cats.xml.modifier

import cats.{Endo, Monoid}
import cats.xml.{XmlData, XmlNode}
import cats.xml.codec.DataEncoder
import cats.xml.cursor.{NodeCursor, TextCursor}

/** Create a modified copy of input [[XmlNode]]
  */
trait Modifier[T] {
  def apply(value: T): Modifier.Result[T]
}

object Modifier extends ModifierInstances {

  import cats.implicits.*

  type Result[+T] = Either[ModifierFailure, T]

  def fromNodeCursor(
    cursor: NodeCursor,
    modifier: Endo[XmlNode]
  ): Modifier[XmlNode] =
    Modifier(node => {
      val nodeClone = node.copy()
      cursor.focus(nodeClone) match {
        case Right(focus) =>
          focus.mute(modifier)
          focus.asRight
        case Left(failure) =>
          ModifierFailure.CursorFailed(failure).asLeft
      }
    })

  def fromTextCursor[T: DataEncoder](
    cursor: TextCursor,
    modifier: Option[XmlData] => T
  ): Modifier[XmlNode] =
    cursor.lastCursor.modify(n => n.withText(modifier(n.text)))

  def apply[T](
    f: T => Modifier.Result[T]
  ): Modifier[T] = (input: T) => f(input)

  def id[T]: Modifier[T] =
    Modifier(_.asRight)

  def pure[T](value: T): Modifier[T] =
    const(value.asRight)

  def const[T](
    result: => Modifier.Result[T]
  ): Modifier[T] =
    Modifier(_ => result)

  def failed[T](
    result: => ModifierFailure
  ): Modifier[T] =
    const(result.asLeft)
}

sealed trait ModifierInstances {

  implicit def monoidForModifier[T]: Monoid[Modifier[T]] = new Monoid[Modifier[T]] {
    override def empty: Modifier[T] = Modifier.id[T]
    override def combine(x: Modifier[T], y: Modifier[T]): Modifier[T] =
      Modifier[T](node => x.apply(node).flatMap(y(_)))
  }
}
