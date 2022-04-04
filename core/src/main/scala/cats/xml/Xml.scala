package cats.xml

import cats.MonadThrow

trait Xml
object Xml {
  def fromString[F[_]: MonadThrow](xmlString: String)(implicit parser: XmlParser[F]): F[XmlNode] =
    parser.parseString(xmlString)
}
