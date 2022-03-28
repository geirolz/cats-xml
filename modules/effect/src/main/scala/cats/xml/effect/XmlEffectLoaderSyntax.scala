package cats.xml.effect

import cats.effect.{Resource, Sync}
import cats.xml.{Xml, XmlLoader, XmlNode}

import java.io.{File, FileInputStream, InputStream}
import scala.annotation.unused

private[effect] trait XmlEffectLoaderSyntax {

  implicit class XmlEffectLoaderOps[F[_]: Sync](loader: XmlLoader[F]) {

    def loadFile(file: File): Resource[F, XmlNode] =
      loadInputStreamResource(new FileInputStream(file))

    def loadFile(path: String): Resource[F, XmlNode] =
      loadInputStreamResource(new FileInputStream(path))

    def loadResourceFile(path: String): Resource[F, XmlNode] =
      loader.loadInputStreamResource(getClass.getResourceAsStream(path))

    def loadInputStreamResource(inputSource: => InputStream): Resource[F, XmlNode] =
      Resource
        .fromAutoCloseable(Sync[F].delay(inputSource))
        .evalMap(is => Sync[F].defer(loader.fromInputStream(is)))
  }

  implicit class XmlObjEffectLoaderOps(@unused loader: Xml.type) {

    def loadFile[F[_]: Sync](file: File)(implicit
      loader: XmlLoader[F]
    ): Resource[F, Xml] =
      loader.loadFile(file)

    def loadFile[F[_]: Sync: XmlLoader](path: String)(implicit
      loader: XmlLoader[F]
    ): Resource[F, Xml] =
      loader.loadFile(path)

    def loadResourceFile[F[_]: Sync: XmlLoader](path: String)(implicit
      loader: XmlLoader[F]
    ): Resource[F, Xml] =
      loader.loadResourceFile(path)

    def loadInputStreamResource[F[_]: Sync](inputSource: => InputStream)(implicit
      loader: XmlLoader[F]
    ): Resource[F, Xml] =
      loader.loadInputStreamResource(inputSource)
  }
}
