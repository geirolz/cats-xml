package cats.xml.generic

sealed trait LabelCase
object LabelCase {
  case object CamelCase extends LabelCase
  case object KebabCase extends LabelCase
  case object PascalCase extends LabelCase
}
