package cats.xml.cursor

trait GenericCursor[I, +O] {
  def focus(input: I): Cursor.Result[O]
}
