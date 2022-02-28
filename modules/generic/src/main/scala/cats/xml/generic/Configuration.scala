package cats.xml.generic

sealed trait ManualTypeSelector
object ManualTypeSelector {
  case object Attr extends ManualTypeSelector
  case object Node extends ManualTypeSelector
//  case object Text extends ManualTypeSelector
}

case class node() extends scala.annotation.StaticAnnotation
case class attr() extends scala.annotation.StaticAnnotation
//final class textOfNode extends scala.annotation.StaticAnnotation with ManualTypeSelector

case class Configuration(
  caseSensitive: Boolean = true,
  trimText: Boolean      = true
)

object Configuration {
  lazy val default: Configuration = Configuration()
}
