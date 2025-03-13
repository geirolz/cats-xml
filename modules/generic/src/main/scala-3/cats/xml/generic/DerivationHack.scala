package cats.xml.generic

import magnolia1.Macro.{anns, inheritedAnns, isEnum, isObject, paramTypeAnns, typeInfo}
import magnolia1.{CallByNeed, CaseClass, CaseClassDerivation, SealedTrait}

import scala.annotation.nowarn
import scala.compiletime.{erasedValue, summonFrom, summonInline}
import scala.deriving.Mirror

private trait SerializableFunction0[+R] extends Function0[R] with Serializable:
  def apply(): R

private trait SerializableFunction1[-T1, +R] extends Function1[T1, R] with Serializable:
  def apply(v1: T1): R

// Modified version of magnolia1.Derivation (and relevant parents) which permits configuration of the produced
// `TypeClass[T]` by summoning an implicit `Params[T]` (which represents our internal `XmlTypeInterpreter`)
trait DerivationHack[TypeClass[_], Params[_]] {
  final type Ps[S] = Params[S]
  protected inline def sealedTraitFromMirror[A](
    m: Mirror.SumOf[A]
  ): SealedTrait[Typeclass, A] =
    SealedTrait(
      typeInfo[A],
      IArray(subtypesFromMirror[A, m.MirroredElemTypes](m)*),
      IArray.from(anns[A]),
      IArray(paramTypeAnns[A]*),
      isEnum[A],
      IArray.from(inheritedAnns[A])
    )

  @nowarn protected transparent inline def subtypesFromMirror[A, SubtypeTuple <: Tuple](
    m: Mirror.SumOf[A],
    result: List[SealedTrait.Subtype[Typeclass, A, _]] = Nil
  ): List[SealedTrait.Subtype[Typeclass, A, _]] =
    inline erasedValue[SubtypeTuple] match
      case _: EmptyTuple =>
        result.distinctBy(_.typeInfo).sortBy(_.typeInfo.full)
      case _: (s *: tail) =>
        val sub = summonFrom {
          case mm: Mirror.SumOf[`s`] =>
            subtypesFromMirror[A, mm.MirroredElemTypes](
              mm.asInstanceOf[m.type],
              Nil
            )
          case _ => {
            val tc = new SerializableFunction0[Typeclass[s]]:
              override def apply(): Typeclass[s] = summonFrom {
                case tc: Typeclass[`s`] => tc
                case _ => deriveSubtype(summonInline[Mirror.Of[s]], summonInline[Params[s]])
              }
            val isType = new SerializableFunction1[A, Boolean]:
              override def apply(a: A): Boolean = a.isInstanceOf[s & A]
            val asType = new SerializableFunction1[A, s & A]:
              override def apply(a: A): s & A = a.asInstanceOf[s & A]
            List(
              new SealedTrait.Subtype[Typeclass, A, s](
                typeInfo[s],
                IArray.from(anns[s]),
                IArray.from(inheritedAnns[s]),
                IArray.from(paramTypeAnns[A]),
                isObject[s],
                0, // unused
                CallByNeed.createLazy(tc),
                isType,
                asType
              )
            )
          }
        }
        subtypesFromMirror[A, tail](m, sub ::: result)
  // From CommonDerivation
  type Typeclass[T] = TypeClass[T]

  inline def getParams[T, Labels <: Tuple, Params <: Tuple](
    annotations: Map[String, List[Any]],
    inheritedAnnotations: Map[String, List[Any]],
    typeAnnotations: Map[String, List[Any]],
    repeated: Map[String, Boolean],
    defaults: Map[String, Option[() => Any]]
  ): List[CaseClass.Param[Typeclass, T]] = CaseClassDerivation.paramsFromMaps(
    annotations,
    inheritedAnnotations,
    typeAnnotations,
    repeated,
    defaults
  )

  def join[T: Params](caseClass: CaseClass[Typeclass, T]): Typeclass[T]

  inline def derivedMirrorProduct[A: Params](
    product: Mirror.ProductOf[A]
  ): Typeclass[A] = join(CaseClassDerivation.fromMirror(product))

  // From Derivation

  def split[T: Params](sealedTrait: SealedTrait[Typeclass, T]): Typeclass[T]

  transparent inline def subtypes[T, SubtypeTuple <: Tuple](
    m: Mirror.SumOf[T]
  ): List[SealedTrait.Subtype[Typeclass, T, _]] =
    subtypesFromMirror[T, SubtypeTuple](m)

  inline def derivedMirrorSum[A: Params](sum: Mirror.SumOf[A]): Typeclass[A] =
    split(sealedTraitFromMirror(sum))

  inline def derivedMirror[A](using mirror: Mirror.Of[A], i: Params[A]): Typeclass[A] =
    inline mirror match
      case sum: Mirror.SumOf[A]         => derivedMirrorSum[A](sum)
      case product: Mirror.ProductOf[A] => derivedMirrorProduct[A](product)

  protected inline def deriveSubtype[s](
    m: Mirror.Of[s],
    i: Params[s]
  ): Typeclass[s] = derivedMirror[s](using m, i)

  // for `derives` syntax, seems like we can't take the Params config in the defn signature
  inline def derived[A](using mirror: Mirror.Of[A]): Typeclass[A] =
    summonFrom {
      case p: Params[A] => derivedMirror[A](using mirror, p)
      case _            => derivedMirror[A](using mirror, default[A])
    }

  def default[A]: Params[A] = throw new IllegalStateException("No default implemented")

  inline def derived[T <: AnyVal & Product: Params]: TypeClass[T] = handleAnyVal[T]

  inline def handleAnyVal[A <: AnyVal & Product: Params]: TypeClass[A]
  inline def handlePrimitive[A]: TypeClass[A]

  // alias because I can't write macros well enough to avoid it r/n
  inline def mirrorDerived[A](using mirror: Mirror.Of[A]): Typeclass[A] = derived[A]
  inline def noMirrorDerived[A]: Typeclass[A]                           = handlePrimitive[A]
}

trait AutoDerivationHack[TypeClass[_], Params[_]] extends DerivationHack[TypeClass, Params]:
  inline given autoDerived[A](using Mirror.Of[A], Params[A]): TypeClass[A] = derivedMirror[A]
  inline given autoDerived[A <: AnyVal & Product: Params]: TypeClass[A]    = derived[A]
