package cats.xml.cursor

trait GenericCursor[A, +B] {
  def focus(x: A): CursorResult[B]
}
