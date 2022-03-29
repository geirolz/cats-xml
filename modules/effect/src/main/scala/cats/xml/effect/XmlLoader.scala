package cats.xml.effect

import cats.effect.{Resource, Sync}
import cats.xml.{XmlNode, XmlParser}

import java.io.{File, FileInputStream, InputStream}

trait XmlLoader[F[_]] {

  def loadInputStreamResource(inputStream: => InputStream): Resource[F, XmlNode]

  def loadFile(file: File): Resource[F, XmlNode] =
    loadInputStreamResource(new FileInputStream(file))

  def loadFile(path: String): Resource[F, XmlNode] =
    loadInputStreamResource(new FileInputStream(path))

  def loadResourceFile(path: String): Resource[F, XmlNode] =
    loadInputStreamResource(getClass.getResourceAsStream(path))
}
object XmlLoader extends XmlLoaderInstances {

  def apply[F[_]](implicit loader: XmlLoader[F]): XmlLoader[F] = loader

  def withParser[F[_]](parser: XmlParser[F])(implicit F: Sync[F]): XmlLoader[F] =
    new XmlLoader[F] {
      override def loadInputStreamResource(inputStream: => InputStream): Resource[F, XmlNode] =
        Resource
          .fromAutoCloseable(F.delay(inputStream))
          .evalMap(is => F.defer(parser.parseInputStream(is)))
    }
}
private[effect] trait XmlLoaderInstances {

  implicit def xmlLoaderWithParser[F[_]: Sync](implicit parser: XmlParser[F]): XmlLoader[F] =
    XmlLoader.withParser(parser)
}
