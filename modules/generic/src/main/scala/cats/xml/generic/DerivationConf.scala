package cats.xml.generic

case class DerivationConf(
  attributesLabelCase: LabelCase,
  nodesLabelCase: LabelCase
)

object DerivationConf {
  val default: DerivationConf = DerivationConf(
    attributesLabelCase = LabelCase.CamelCase,
    nodesLabelCase      = LabelCase.CamelCase
  )
}
