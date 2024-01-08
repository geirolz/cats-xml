# Standard Scala XML support

Add standard scala XML interop support.

```sbt
libraryDependencies += "com.github.geirolz" %% "cats-xml-scalaxml" % "@VERSION@"
```

Use

```scala mdoc:nest:to-string
import cats.xml.scalaxml.implicits.*
```

To have all the conversion method to transform a cats-xml object into std scala xml.
For example, you could convert a `XmlNode` to a `NodeSeq` using `toNodeSeq`

This implicitly transform the `NodeSeq` to `XmlNode`

```scala mdoc
import cats.xml.XmlNode

val xmlNode: XmlNode = <Wrapper><Root><Value>100</Value></Root></Wrapper>
```
