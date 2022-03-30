package cats.xml.effect

import cats.effect.Resource
import cats.xml.Xml

import java.io.{File, InputStream}
import scala.annotation.unused

object implicits extends AllInstances with AllSyntax

object instances extends AllInstances
sealed trait AllInstances extends XmlLoaderInstances

object syntax extends AllSyntax
sealed trait AllSyntax extends XmlObjLoaderEffectSyntax

private[effect] trait XmlObjLoaderEffectSyntax {

  implicit class XmlObjLoaderEffectOps(@unused xmlObj: Xml.type) {

    def loadFile[F[_]: XmlLoader](file: File): Resource[F, Xml] =
      XmlLoader[F].loadFile(file)

    def loadFile[F[_]: XmlLoader](path: String): Resource[F, Xml] =
      XmlLoader[F].loadFile(path)

    def loadResourceFile[F[_]: XmlLoader](path: String): Resource[F, Xml] =
      XmlLoader[F].loadResourceFile(path)

    def loadInputStreamResource[F[_]: XmlLoader](inputSource: => InputStream): Resource[F, Xml] =
      XmlLoader[F].loadInputStreamResource(inputSource)
  }
}
