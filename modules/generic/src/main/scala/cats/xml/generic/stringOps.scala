package cats.xml.generic

import cats.xml.generic.StringMapper.*

sealed trait StringMapper extends (String => String) {

  import StringOps.*

  override def apply(value: String): String = {
    val split: Array[String] = value.inclusiveSplit(Array('_', '-'))
    this match {
      case CamelCase =>
        split.zipWithIndex
          .map { case (word: String, index: Int) =>
            if (index != 0) word.capitalize else word
          }
          .mkString
          .capitalize
      case KebabCase  => split.mkString("-")
      case SnakeCase  => split.mkString("_")
      case PascalCase => split.map(_.capitalize).mkString
    }
  }
}
object StringMapper {
  case object CamelCase extends StringMapper
  case object KebabCase extends StringMapper
  case object SnakeCase extends StringMapper
  case object PascalCase extends StringMapper
}

object StringOps extends StringOpsSyntax {

  def inclusiveSplit(value: String, splitChars: Array[Char]): Array[String] =
    value
      .foldLeft(Array.empty[Array[Char]])((acc: Array[Array[Char]], value: Char) => {
        if (value.isUpper || splitChars.contains(value))
          acc.appended(Array(value))
        else {
          acc.lastOption match {
            case Some(lastChunk) => acc.updated(acc.length - 1, lastChunk.appended(value))
            case None            => acc.appended(Array(value))
          }
        }
      })
      .map(_.mkString)
}
trait StringOpsSyntax {

  implicit class stringCustomOps(value: String) {
    def inclusiveSplit(splitChars: Array[Char]): Array[String] =
      StringOps.inclusiveSplit(value, splitChars)
  }
}
