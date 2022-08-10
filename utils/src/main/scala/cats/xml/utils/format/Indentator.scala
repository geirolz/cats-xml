package cats.xml.utils.format

case class Indentator(char: Char, size: Int, depth: Int, indentation: String) {

  private val unit = Indentator.buildString(char, size, 1)

  def forward: Indentator = this.copy(
    depth       = depth + 1,
    indentation = indentation + unit
  )

  def backward: Indentator = this.copy(
    depth       = depth - 1,
    indentation = if (indentation.nonEmpty) indentation.drop(1) else indentation
  )
}
object Indentator {

  def root(char: Char, size: Int): Indentator =
    build(
      char  = char,
      size  = size,
      depth = 0
    )

  def build(char: Char, size: Int, depth: Int): Indentator =
    Indentator(
      char        = char,
      size        = size,
      depth       = depth,
      indentation = (0 until size * depth).map(_ => char).mkString
    )

  def buildString(char: Char, size: Int, depth: Int): String =
    (0 until size * depth).map(_ => char).mkString
}
