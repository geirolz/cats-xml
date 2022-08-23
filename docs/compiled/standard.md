# Standard Scala XML support

Add standard scala XML interop support.

```sbt
libraryDependencies += "com.github.geirolz" %% "cats-xml-standard" % "0.0.3"
```

Use
```scala
import cats.xml.std.implicits.*
```

To have all the conversion method to transform a cats-xml object into std scala xml on.
For example, you could convert a `XmlNode` to a `NodeSeq` using `toNodeSeq`