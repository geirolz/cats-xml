import cats.xml.utils.generic.TypeInfo
import cats.xml.utils.generic._

val t:  TypeInfo[Foo] = TypeInfo.auto.deriveTypeInfo[Foo]


//val t: Map[ParamName[Foo], TypeInfo[_]] = TypeInfo.auto.deriveFieldsTypeInfo[Foo]





