# Encoder and Decoder derivation

At the moment supported only for Scala 2.

```sbt
libraryDependencies += "com.github.geirolz" %% "cats-xml-generic" % "@VERSION@"
```     
                    
## XmlTypeInterpreter
`XmlTypeInterpreter` is used to map fields and get the xml type and label.
By default using `XmlTypeInterpreter.default[T]` or using it implicitly
- `Attribute`:
  - type is primitive
  - type is a primitive wrapper (BigInt, BigDecimal)
  - type is a value class
- `Text`
  - no fields are treated as `Text` 
- `Child`
  - If it is not neither `Attribute` nor `Text`

## Derivation

Given 
```scala mdoc:reset-object
case class ValueClass(value: String) extends AnyVal
case class Bar(field1: String, field2: BigDecimal)
case class Foo(
  primitiveField: Double = 666d,
  valueClass: ValueClass,
  bar: Bar,
  missingField: Option[String],
  missingNode: Option[Bar]
)
```

### Decoder semiauto

```scala mdoc:nest:to-string
import cats.xml.XmlNode
import cats.xml.codec.Decoder
import cats.xml.generic.{XmlElemType, XmlTypeInterpreter}

import cats.xml.syntax.*
import cats.xml.generic.decoder.semiauto.*

implicit val typeInterpreterFoo: XmlTypeInterpreter[Foo] =
  XmlTypeInterpreter
          .default[Foo]
          .overrideType(
            _.param(_.valueClass) -> XmlElemType.Attribute
          )

implicit val decoderValueClass: Decoder[ValueClass] = deriveDecoder[ValueClass]
implicit val decoderBar: Decoder[Bar] = deriveDecoder[Bar]
implicit val decoderFoo: Decoder[Foo] = deriveDecoder[Foo]

XmlNode("foo")
        .withAttrs(
          "primitiveField" := 1d,
          "valueClass" := "TEST"
        )
        .withChildren(
          XmlNode("bar")
                  .withAttrs(
                    "field1" := "BHO",
                    "field2" := BigDecimal(100)
                  )
        )
        .as[Foo]
```

### Encoder semiauto
```scala mdoc:nest:to-string
import cats.xml.codec.Encoder
import cats.xml.generic.{XmlElemType, XmlTypeInterpreter}

import cats.xml.syntax.*
import cats.xml.generic.encoder.semiauto.*

implicit val typeInterpreterFoo: XmlTypeInterpreter[Foo] =
  XmlTypeInterpreter
    .default[Foo]
    .overrideType(
      _.param(_.valueClass) -> XmlElemType.Attribute
    )

implicit val encoderValueClass: Encoder[ValueClass] = deriveEncoder[ValueClass]
implicit val encoderBar: Encoder[Bar]               = deriveEncoder[Bar]
implicit val encoderFoo: Encoder[Foo]               = deriveEncoder[Foo]

Foo(
  primitiveField = 1d,
  valueClass     = ValueClass("TEST"),
  bar            = Bar("BHO", BigDecimal(100)),
  missingField   = None,
  missingNode    = None
).toXml
```