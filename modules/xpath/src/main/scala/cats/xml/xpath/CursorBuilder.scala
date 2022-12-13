package cats.xml.xpath

import cats.parse.Parser
import cats.xml.cursor.NodeCursor
import cats.xml.cursor.NodeCursor.Root
import cats.xml.xpath.error.XPathError
import cats.xml.XmlNode
import cats.Endo
import eu.cdevreeze.xpathparser.ast.*
import eu.cdevreeze.xpathparser.common.{PrefixedName, UnprefixedName}
import eu.cdevreeze.xpathparser.parse.XPathParser

object CursorBuilder {

  def fromXPath(xpathValue: String): Either[XPathError, NodeCursor] =
    XPathParser.xpathExpr.parse(xpathValue) match {
      case Left(value: Parser.Error) =>
        Left(XPathError.ParsingError(value))
      case Right((_, xpathExpr)) =>
        CursorBuilder.fromXPath(xpathExpr)
    }

  def fromXPath(xpathExpr: XPathExpr): Either[XPathError, NodeCursor] = {
    xpathExpr match {
      case expr: Expr => cursorModifierFromExpr(expr).map(_.apply(Root))
    }
  }

  // ---------------------------------------------//
  private def notSupported[T](
    feature: XPathElem
  ): Either[XPathError, T] =
    Left(XPathError.NotSupportedConstruction(feature))

  private def cursorModifierFromForwardStep(
    step: ForwardStep
  ): Either[XPathError, Endo[NodeCursor]] =
    step match {
      case nafs @ NonAbbrevForwardStep(_, _) => notSupported(nafs)
      case step: AbbrevForwardStep =>
        step match {
          case SimpleAbbrevForwardStep(nodeTest)         => cursorModifierFromNodeTest(nodeTest)
          case aaafs @ AttributeAxisAbbrevForwardStep(_) => notSupported(aaafs)
        }
    }

  private def cursorModifierFromNodeTest(test: NodeTest): Either[XPathError, Endo[NodeCursor]] =
    test match {
      case test: KindTest =>
        notSupported(test)
      case test: NameTest =>
        test match {
          case snt @ SimpleNameTest(name) =>
            name match {
              case EQName.QName(qname) =>
                qname match {
                  case UnprefixedName(localPart) =>
                    Right(NodeCursor.endo(_.down(localPart)))
                  case PrefixedName(_, _) => notSupported(snt)
                }
              case EQName.URIQualifiedName(_) => notSupported(snt)
            }
          case wildcard: Wildcard =>
            wildcard match {
              case _ @AnyWildcard             => Right(NodeCursor.endo(_.downWildcard))
              case pw @ PrefixWildcard(_)     => notSupported(pw)
              case lnw @ LocalNameWildcard(_) => notSupported(lnw)
              case nw @ NamespaceWildcard(_)  => notSupported(nw)
            }
        }
    }

  private def cursorModifierFromStepExpr(
    stepExpr: StepExpr
  ): Either[XPathError, Endo[NodeCursor]] =
    stepExpr match {
      case postfixExpr: PostfixExpr => notSupported(postfixExpr)
      case axisStep: AxisStep       => cursorModifierFromAxisStep(axisStep)
    }

  private def cursorModifierFromAxisStep(step: AxisStep): Either[XPathError, Endo[NodeCursor]] =
    step match {
      case ForwardAxisStep(step, predicateList) =>
        predicateList.foldLeft(cursorModifierFromForwardStep(step)) { case (res, predicate) =>
          res.flatMap(f => cursorModifierFromPostfix(predicate).map(f.andThen))
        }
      case ras @ ReverseAxisStep(_, _) => notSupported(ras)
    }

  private def cursorModifierFromExpr(expr: Expr): Either[XPathError, Endo[NodeCursor]] =
    expr match {
      case simpleExpr: SimpleExpr =>
        simpleExpr match {
          case single: ExprSingle =>
            single match {
              case fe @ ForExpr(_, _)           => notSupported(fe)
              case le @ LetExpr(_, _)           => notSupported(le)
              case qe @ QuantifiedExpr(_, _, _) => notSupported(qe)
              case ie @ IfExpr(_, _, _)         => notSupported(ie)
              case orExpr: OrExpr               => cursorModifierFromOrExpr(orExpr)
            }
        }
      case ce @ CompoundExpr(_, _) => notSupported(ce)
    }

  private def cursorModifierFromOrExpr(orExpr: OrExpr): Either[XPathError, Endo[NodeCursor]] =
    orExpr match {
      case simpleOrExpr: SimpleOrExpr =>
        simpleOrExpr match {
          case andExpr: AndExpr => cursorModifierFromAndExpr(andExpr)
        }
      case coe @ CompoundOrExpr(_, _) => notSupported(coe)
    }

  private def cursorModifierFromAndExpr(
    andExpr: AndExpr
  ): Either[XPathError, Endo[NodeCursor]] =
    andExpr match {
      case simpleAndExpr: SimpleAndExpr =>
        simpleAndExpr match {
          case comparisonExpr: ComparisonExpr => cursorModifierFromComparisonExpr(comparisonExpr)
        }
      case cae @ CompoundAndExpr(_, _) => notSupported(cae)
    }

  private def cursorModifierFromComparisonExpr(
    comparisonExpr: ComparisonExpr
  ): Either[XPathError, Endo[NodeCursor]] =
    comparisonExpr match {
      case simpleComparisonExpr: SimpleComparisonExpr =>
        simpleComparisonExpr match {
          case stringConcatExpr: StringConcatExpr =>
            cursorModifierFromStringConcatExpr(stringConcatExpr)
        }

      case cce @ CompoundComparisonExpr(_, _, _) =>
        notSupported(cce)
    }

  private def cursorModifierFromStringConcatExpr(
    stringConcatExpr: StringConcatExpr
  ): Either[XPathError, Endo[NodeCursor]] =
    stringConcatExpr match {
      case simpleStringConcatExpr: SimpleStringConcatExpr =>
        simpleStringConcatExpr match {
          case rangeExpr: RangeExpr => cursorModifierFromRangeExpr(rangeExpr)
        }
      case csce @ CompoundStringConcatExpr(_, _) => notSupported(csce)
    }

  private def cursorModifierFromRangeExpr(
    rangeExpr: RangeExpr
  ): Either[XPathError, Endo[NodeCursor]] =
    rangeExpr match {
      case simpleRangeExpr: SimpleRangeExpr =>
        simpleRangeExpr match {
          case additiveExpr: AdditiveExpr => cursorModifierFromAdditiveExpr(additiveExpr)
        }
      case cre @ CompoundRangeExpr(_, _) => notSupported(cre)
    }

  private def cursorModifierFromAdditiveExpr(
    additiveExpr: AdditiveExpr
  ): Either[XPathError, Endo[NodeCursor]] =
    additiveExpr match {
      case simpleAdditiveExpr: SimpleAdditiveExpr =>
        simpleAdditiveExpr match {
          case multiplicativeExpr: MultiplicativeExpr =>
            cursorModifierFromMultiplicativeExpr(multiplicativeExpr)
        }
      case cae @ CompoundAdditiveExpr(_, _, _) =>
        notSupported(cae)
    }

  private def cursorModifierFromMultiplicativeExpr(
    multiplicativeExpr: MultiplicativeExpr
  ): Either[XPathError, Endo[NodeCursor]] =
    multiplicativeExpr match {
      case simpleMultiplicativeExpr: SimpleMultiplicativeExpr =>
        simpleMultiplicativeExpr match {
          case unionExpr: UnionExpr => cursorModifierFromUnionExpr(unionExpr)
        }
      case cme @ CompoundMultiplicativeExpr(_, _, _) =>
        notSupported(cme)
    }

  private def cursorModifierFromUnionExpr(
    unionExpr: UnionExpr
  ): Either[XPathError, Endo[NodeCursor]] =
    unionExpr match {
      case simpleUnionExpr: SimpleUnionExpr =>
        simpleUnionExpr match {
          case intersectExceptExpr: IntersectExceptExpr =>
            cursorModifierFromIntersectExceptExpr(intersectExceptExpr)
        }
      case cue @ CompoundUnionExpr(_, _) => notSupported(cue)
    }

  private def cursorModifierFromIntersectExceptExpr(
    intersectExceptExpr: IntersectExceptExpr
  ): Either[XPathError, Endo[NodeCursor]] =
    intersectExceptExpr match {
      case simpleIntersectExceptExpr: SimpleIntersectExceptExpr =>
        simpleIntersectExceptExpr match {
          case instanceOfExpr: InstanceOfExpr =>
            cursorModifierFromInstanceOfExpr(instanceOfExpr)
        }
      case ciee @ CompoundIntersectExceptExpr(_, _, _) =>
        notSupported(ciee)
    }

  private def cursorModifierFromInstanceOfExpr(
    instanceOfExpr: InstanceOfExpr
  ): Either[XPathError, Endo[NodeCursor]] =
    instanceOfExpr match {
      case simpleInstanceOfExpr: SimpleInstanceOfExpr =>
        simpleInstanceOfExpr match {
          case treatExpr: TreatExpr => cursorModifierFromTreatExpr(treatExpr)
        }
      case cioe @ CompoundInstanceOfExpr(_, _) => notSupported(cioe)
    }

  private def cursorModifierFromTreatExpr(
    treatExpr: TreatExpr
  ): Either[XPathError, Endo[NodeCursor]] =
    treatExpr match {
      case simpleTreatExpr: SimpleTreatExpr =>
        simpleTreatExpr match {
          case castableExpr: CastableExpr => cursorModifierFromCastableExpr(castableExpr)
        }
      case cte @ CompoundTreatExpr(_, _) => notSupported(cte)
    }

  private def cursorModifierFromCastableExpr(
    castableExpr: CastableExpr
  ): Either[XPathError, Endo[NodeCursor]] =
    castableExpr match {
      case simpleCastableExpr: SimpleCastableExpr =>
        simpleCastableExpr match {
          case castExpr: CastExpr => cursorModifierFromCastExpr(castExpr)
        }
      case cce @ CompoundCastableExpr(_, _) => notSupported(cce)
    }

  private def cursorModifierFromCastExpr(
    castExpr: CastExpr
  ): Either[XPathError, Endo[NodeCursor]] =
    castExpr match {
      case simpleCastExpr: SimpleCastExpr =>
        simpleCastExpr match {
          case arrowExpr: ArrowExpr => cursorModifierFromArrowExpr(arrowExpr)
        }
      case cce @ CompoundCastExpr(_, _) => notSupported(cce)
    }

  private def cursorModifierFromArrowExpr(
    arrowExpr: ArrowExpr
  ): Either[XPathError, Endo[NodeCursor]] =
    arrowExpr match {
      case simpleArrowExpr: SimpleArrowExpr =>
        simpleArrowExpr match {
          case unaryExpr: UnaryExpr => cursorModifierFromUnaryExpr(unaryExpr)
        }
      case cae @ CompoundArrowExpr(_, _) => notSupported(cae)
    }

  private def cursorModifierFromUnaryExpr(
    unaryExpr: UnaryExpr
  ): Either[XPathError, Endo[NodeCursor]] =
    unaryExpr match {
      case simpleUnaryExpr: SimpleUnaryExpr =>
        simpleUnaryExpr match {
          case valueExpr: ValueExpr => cursorModifierFromValueExpr(valueExpr)
        }
      case cue @ CompoundUnaryExpr(_, _) => notSupported(cue)
    }

  private def cursorModifierFromValueExpr(
    valueExpr: ValueExpr
  ): Either[XPathError, Endo[NodeCursor]] =
    valueExpr match {
      case simpleMapExpr: SimpleMapExpr =>
        simpleMapExpr match {
          case simpleSimpleMapExpr: SimpleSimpleMapExpr =>
            simpleSimpleMapExpr match {
              case pathExpr: PathExpr => cursorModifierFromPathExpr(pathExpr)
            }
          case csme @ CompoundSimpleMapExpr(_, _) => notSupported(csme)
        }
    }

  private def cursorModifierFromPathExpr(
    pathExpr: PathExpr
  ): Either[XPathError, Endo[NodeCursor]] =
    pathExpr match {
      case SlashOnlyPathExpr => Right(identity)
      case PathExprStartingWithSingleSlash(relativePathExpr) =>
        cursorModifierFromRelativePathExpr(relativePathExpr)
      case peswds @ PathExprStartingWithDoubleSlash(_) => notSupported(peswds)
      case relativePathExpr: RelativePathExpr =>
        cursorModifierFromRelativePathExpr(relativePathExpr)
    }

  private def cursorModifierFromRelativePathExpr(
    relativePathExpr: RelativePathExpr
  ): Either[XPathError, Endo[NodeCursor]] =
    relativePathExpr match {
      case simpleRelativePathExpr: SimpleRelativePathExpr =>
        simpleRelativePathExpr match {
          case stepExpr: StepExpr => cursorModifierFromStepExpr(stepExpr)
        }

      case crpe @ CompoundRelativePathExpr(_, StepOp.DoubleSlash, _) =>
        notSupported(crpe)

      case CompoundRelativePathExpr(init, StepOp.SingleSlash, lastStepExpr) =>
        cursorModifierFromRelativePathExpr(init)
          .flatMap(i => cursorModifierFromStepExpr(lastStepExpr).map(i.andThen))
    }

  private def cursorModifierFromPostfix(
    postfix: Postfix
  ): Either[XPathError, Endo[NodeCursor]] =
    postfix match {
      case Predicate(IntegerLiteral(v)) =>
        Right(NodeCursor.endo(_.atIndex(v.toInt)))
      case Predicate(FunctionCall(EQNameEx("last"), ArgumentList(EmptySeq()))) =>
        Right(NodeCursor.endo(_.last))
      case Predicate(expr) =>
        PredicateBuilder.fromExpr(expr).map(p => NodeCursor.endo(_.filter(p)))
      case al @ ArgumentList(_) =>
        notSupported(al)
      case pl @ PostfixLookup(_) =>
        notSupported(pl)
    }

  object PredicateBuilder {

    import cats.syntax.apply.*
    import cats.syntax.foldable.*
    import cats.syntax.traverse.*
    import eu.cdevreeze.xpathparser.ast.*

    def fromExpr(expr: Expr): Either[XPathError, XmlNode => Boolean] =
      expr match {
        case expr: CompoundComparisonExpr =>
          fromCompoundComparisonExpr(expr)
        case expr: RelativePathExpr =>
          fromRelativePathExpr(expr).map(_(_ => true))
        case CompoundOrExpr(head, tail) =>
          import cats.xml.xpath.utils.predicate.or.*
          (fromExpr(head), tail.traverse(fromExpr(_))).mapN(_ +: _).map(_.combineAll)
        case CompoundAndExpr(head, tail) =>
          import cats.xml.xpath.utils.predicate.and.*
          (fromExpr(head), tail.traverse(fromExpr(_))).mapN(_ +: _).map(_.combineAll)
        case e => notSupported(e)
      }

    private def fromCompoundComparisonExpr(
      expr: CompoundComparisonExpr
    ): Either[XPathError, XmlNode => Boolean] =
      expr match {
        case CompoundComparisonExpr(
              cpe @ CompoundOrExact(
                ForwardAxisStep(
                  AttributeAxisAbbrevForwardStep(SimpleNameTest(EQNameEx(name))),
                  EmptySeq()
                )
              ),
              GeneralComp.Eq,
              StringLiteral(value)
            ) =>
          fromRelativePathExpr(cpe)
            .map(pModifier => pModifier(_.hasAllAttributes(name -> value)))

        case CompoundComparisonExpr(
              cpe @ CompoundOrExact(
                ForwardAxisStep(SimpleAbbrevForwardStep(SimpleNameTest(_)), EmptySeq())
              ),
              comp: GeneralComp,
              IntegerLiteral(value)
            ) =>
          fromRelativePathExpr(cpe)
            .map[XmlNode => Boolean](pModifier =>
              pModifier(
                _.text
                  .flatMap(_.as[Int].toOption)
                  .exists(comp match {
                    case GeneralComp.Eq => _ == value.toInt
                    case GeneralComp.Ne => _ != value.toInt
                    case GeneralComp.Lt => _ < value.toInt
                    case GeneralComp.Le => _ <= value.toInt
                    case GeneralComp.Gt => _ > value.toInt
                    case GeneralComp.Ge => _ >= value.toInt
                  })
              )
            )

        case CompoundComparisonExpr(
              cpe @ CompoundOrExact(ForwardAxisStep(SimpleAbbrevForwardStep(TextTest), EmptySeq())),
              GeneralComp.Eq,
              StringLiteral(value)
            ) =>
          fromRelativePathExpr(cpe).map(pModifier => pModifier(_.textString == value))

        case e => notSupported(e)
      }

    private def fromRelativePathExpr(
      expr: RelativePathExpr
    ): Either[XPathError, Endo[XmlNode => Boolean]] =
      expr match {
        case ForwardAxisStep(SimpleAbbrevForwardStep(SimpleNameTest(EQNameEx(name))), EmptySeq()) =>
          Right(xpred(p => _.hasChild(name, p)))

        case CompoundRelativePathExpr(initExpr: RelativePathExpr, StepOp.SingleSlash, lastStep) =>
          // check the order
          for {
            init <- fromRelativePathExpr(initExpr)
            last <- fromRelativePathExpr(lastStep)
          } yield last.andThen(init)

        case ForwardAxisStep(SimpleAbbrevForwardStep(TextTest), EmptySeq()) =>
          Right(xpred(identity))

        case ForwardAxisStep(AttributeAxisAbbrevForwardStep(SimpleNameTest(_)), EmptySeq()) =>
          Right(xpred(identity))

        case FunctionCall(
              EQNameEx(fn @ ("contains" | "starts-with" | "ends-with")),
              ArgumentList(
                UnSeq(
                  ExprSingleArgument(
                    ForwardAxisStep(SimpleAbbrevForwardStep(TextTest), EmptySeq())
                  ),
                  ExprSingleArgument(StringLiteral(value))
                )
              )
            ) =>
          fn match {
            case "contains"    => Right(xpredConst(_.textString.contains(value)))
            case "starts-with" => Right(xpredConst(_.textString.startsWith(value)))
            case "ends-with"   => Right(xpredConst(_.textString.endsWith(value)))
          }

        case FunctionCall(
              EQNameEx(fn @ ("contains" | "starts-with" | "ends-with")),
              ArgumentList(
                UnSeq(
                  ExprSingleArgument(
                    ForwardAxisStep(
                      AttributeAxisAbbrevForwardStep(SimpleNameTest(EQNameEx(attrKey))),
                      EmptySeq()
                    )
                  ),
                  ExprSingleArgument(StringLiteral(value))
                )
              )
            ) =>
          fn match {
            case "contains" =>
              Right(xpredConst(_.hasAllAttributes(_.exists(attrKey)(_.contains(value)))))
            case "starts-with" =>
              Right(xpredConst(_.hasAllAttributes(_.exists(attrKey)(_.startsWith(value)))))
            case "ends-with" =>
              Right(xpredConst(_.hasAllAttributes(_.exists(attrKey)(_.endsWith(value)))))
          }

        case FunctionCall(
              EQNameEx("not"),
              ArgumentList(
                UnSeq(
                  ExprSingleArgument(fc: FunctionCall)
                )
              )
            ) =>
          fromRelativePathExpr(fc).map(_.andThen(_.andThen(res => !res)))

        case e => notSupported(e)
      }

    @inline private def xpred(
      f: Endo[XmlNode => Boolean]
    ): Endo[XmlNode => Boolean] =
      f

    @inline private def xpredConst(
      f: XmlNode => Boolean
    ): Endo[XmlNode => Boolean] =
      _ => f

    private object CompoundOrExact {
      def unapply(expr: RelativePathExpr): Option[StepExpr] =
        expr match {
          case se: StepExpr                   => Some(se)
          case crpe: CompoundRelativePathExpr => Some(crpe.lastStepExpr)
        }
    }
  }
}
