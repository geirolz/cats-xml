# Standard Scala XML support

Add standard scala XML interop support.

```sbt
libraryDependencies += "com.github.geirolz" %% "cats-xml-standard" % "0.0.7"
```

Use
```scala
import cats.xml.std.implicits.*
```

To have all the conversion method to transform a cats-xml object into std scala xml.
For example, you could convert a `XmlNode` to a `NodeSeq` using `toNodeSeq`

This implicitly transform the `NodeSeq` to `XmlNode`

```scala
import cats.xml.XmlNode

val xmlNode: XmlNode = <Wrapper><Root><Value>100</Value></Root></Wrapper>
// xmlNode: XmlNode = <Wrapper>
//  <Root>
//   <Value>100</Value>
//  </Root>
// </Wrapper>
```
