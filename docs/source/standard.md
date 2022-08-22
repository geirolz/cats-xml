# Standard Scala XML support

Add standard scala XML interop support.

```sbt
libraryDependencies += "com.github.geirolz" %% "cats-xml-standard" % "@VERSION@"
```

Use
```scala mdoc:nest:to-string
import cats.xml.std.implicits.*
```

To have all the conversion method to transform a cats-xml object into std scala xml on.
For example, you could convert a `XmlNode` to a `NodeSeq` using `toNodeSeq`