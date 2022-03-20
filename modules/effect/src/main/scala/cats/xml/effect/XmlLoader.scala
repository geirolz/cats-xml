package cats.xml.effect

import cats.effect.{Resource, Sync}
import cats.xml.Xml

import java.io.{ByteArrayInputStream, File, FileInputStream, InputStream}
import java.nio.charset.{Charset, StandardCharsets}
import javax.xml.parsers.{DocumentBuilder, DocumentBuilderFactory}
import scala.util.Try

object XmlLoader {

  import cats.implicits.*

  def fromString[F[_]: Sync](string: String, charset: Charset = StandardCharsets.UTF_8): F[Xml] =
    fromSource[F](new ByteArrayInputStream(string.getBytes(charset))).use(_.pure[F])

  def loadFile[F[_]: Sync](file: File): Resource[F, Xml] =
    fromSource[F](new FileInputStream(file))

  def loadFile[F[_]: Sync](name: String): Resource[F, Xml] =
    fromSource[F](new FileInputStream(name))

  def fromSource[F[_]](inputSource: => InputStream)(implicit F: Sync[F]): Resource[F, Xml] = {

    def parse(inputSource: InputStream): Try[Xml] = Try {

      // Parser that produces DOM object trees from XML content
      val factory: DocumentBuilderFactory = DocumentBuilderFactory.newInstance()

      // Create DocumentBuilder with default configuration//Create DocumentBuilder with default configuration
      val builder: DocumentBuilder = factory.newDocumentBuilder

      // Parse the content to Document object
      Xml.fromJavaxDocument(builder.parse(inputSource))
    }

    Resource
      .fromAutoCloseable(F.delay(inputSource))
      .evalMap(is => F.fromTry(parse(is)))
  }
}
