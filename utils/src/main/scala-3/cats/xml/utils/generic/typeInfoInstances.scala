package cats.xml.utils.generic

trait TypeInfoInstances {

  inline given deriveTypeInfo[T]: TypeInfo[T] =
    ${ TypeInfoMacrosScala3.deriveTypeInfo[T] }

  inline given deriveFieldsTypeInfo[T]: Map[ParamName[T], TypeInfo[?]] =
    ${ TypeInfoMacrosScala3.deriveFieldsTypeInfo[T] }
}
