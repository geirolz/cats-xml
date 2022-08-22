package cats.xml.utils

import cats.xml.utils.StringMapper.*

sealed trait StringMapper extends (String => String) {

  import StringOps.*

  lazy val exclusiveSplitterChars: Array[Char] =
    StringMapper.defaultSplitterSymbolChars

  lazy val inclusiveSplitterChars: Array[Char] =
    StringMapper.defaultSplitterUpperCaseLetterChars

  override def apply(value: String): String = {

    val split: Array[String] = value.advancedSplit(
      inclusiveChars = inclusiveSplitterChars,
      exclusiveChars = exclusiveSplitterChars
    )

    this match {
      case CamelCase =>
        split.zipWithIndex.map { case (word: String, index: Int) =>
          if (index != 0)
            word.capitalize
          else
            word
        }.mkString
      case KebabCase  => split.mkString("-").toLowerCase()
      case SnakeCase  => split.mkString("_").toLowerCase()
      case PascalCase => split.map(_.capitalize).mkString
    }
  }
}
object StringMapper {

  lazy val defaultSplitterSymbolChars: Array[Char] =
    "_-".toCharArray

  lazy val defaultSplitterUpperCaseLetterChars: Array[Char] =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ".toCharArray

  case object CamelCase extends StringMapper
  case object KebabCase extends StringMapper
  case object SnakeCase extends StringMapper
  case object PascalCase extends StringMapper
}

object StringOps extends StringOpsSyntax {

  def advancedSplit(
    value: String,
    inclusiveChars: Array[Char],
    exclusiveChars: Array[Char]
  ): Array[String] =
    value
      .foldLeft(Array.empty[Array[Char]])((acc: Array[Array[Char]], value: Char) => {
        if (inclusiveChars.contains(value))
          acc.appended(Array(value))
        else if (exclusiveChars.contains(value))
          acc.appended(Array.empty[Char])
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
    def advancedSplit(inclusiveChars: Array[Char], exclusiveChars: Array[Char]): Array[String] =
      StringOps.advancedSplit(value, inclusiveChars, exclusiveChars)
  }
}
