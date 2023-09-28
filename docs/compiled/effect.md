# Cats Effect support

Add cats-effect support.

```sbt
libraryDependencies += "com.github.geirolz" %% "cats-xml-effect" % "0.0.13"
```     

Use 
```scala
import cats.xml.effect.implicits.*
```

To have the following methods on `Xml` companion object.
```scala
def loadFile[F[_]: XmlLoader](file: File): Resource[F, Xml]
def loadFile[F[_]: XmlLoader](path: String): Resource[F, Xml]
def loadResourceFile[F[_]: XmlLoader](path: String): Resource[F, Xml]
def loadInputStreamResource[F[_]: XmlLoader](inputSource: => InputStream): Resource[F, Xml] 
```
