package cats.xml.testing

sealed trait DataSize
object DataSize {
  case object S extends DataSize
  case object M extends DataSize
  case object L extends DataSize
  case object XL extends DataSize
}
