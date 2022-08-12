import cats.xml.utils.generic.TypeInfo
import cats.xml.utils.generic._

val t: TypeInfo[Foo] = TypeInfo.deriveTypeInfo[Foo]

//val t: Map[ParamName[Foo], TypeInfo[_]] = TypeInfo.auto.deriveFieldsTypeInfo[Foo]
