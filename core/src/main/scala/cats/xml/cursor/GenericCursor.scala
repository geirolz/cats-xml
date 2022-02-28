package cats.xml.cursor

trait GenericCursor[I, +O] {
  def focus(input: I): CursorResult[O]
}
