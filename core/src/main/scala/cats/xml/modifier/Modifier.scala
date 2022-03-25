package cats.xml.modifier

import cats.xml.{XmlData, XmlNode}
import cats.xml.cursor.{CursorResult, NodeCursor, TextCursor}
import cats.{Endo, Monoid}
import cats.xml.codec.DataEncoder

/** Create a modified copy of input [[XmlNode]]
  */
trait Modifier[T] {
  def apply(value: T): ModifierResult[T]
}

object Modifier extends ModifierInstances {

  def fromNodeCursor(
    cursor: NodeCursor,
    modifier: Endo[XmlNode]
  ): Modifier[XmlNode] =
    Modifier(node => {
      val nodeClone = node.copy()
      cursor.focus(nodeClone) match {
        case failed: CursorResult.Failed => ModifierResult.CursorFailed(failed)
        case CursorResult.Focused(focus) =>
          focus.mute(modifier)
          ModifierResult.Modified(focus)
      }
    })

  def fromTextCursor[T: DataEncoder](
    cursor: TextCursor,
    modifier: Option[XmlData] => T
  ): Modifier[XmlNode] =
    cursor.lastCursor.modify(n => n.withText(modifier(n.text)))

  def apply[T](
    f: T => ModifierResult[T]
  ): Modifier[T] = (input: T) => f(input)

  def id[T]: Modifier[T] = Modifier(ModifierResult.pure)

  def const[T](
    result: => ModifierResult[T]
  ): Modifier[T] =
    Modifier(_ => result)

  def failed[T](
    result: => ModifierResult.ModifierFailed
  ): Modifier[T] =
    const(result)
}

sealed trait ModifierInstances {

  implicit def monoidForModifier[T]: Monoid[Modifier[T]] = new Monoid[Modifier[T]] {
    override def empty: Modifier[T] = Modifier.id[T]
    override def combine(x: Modifier[T], y: Modifier[T]): Modifier[T] = Modifier[T](node => {
      x.apply(node) match {
        case ModifierResult.Modified(value)        => y.apply(value)
        case failed: ModifierResult.ModifierFailed => failed
      }
    })
  }
}
