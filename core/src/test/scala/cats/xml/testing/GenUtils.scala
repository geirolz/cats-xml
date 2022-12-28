package cats.xml.testing

import org.scalacheck.Gen

object GenUtils {

  def getNonEmptyString(maxSize: Int = 10): Gen[String] =
    Gen.lzy(
      Gen
        .choose(1, maxSize)
        .flatMap(Gen.stringOfN(_, Gen.alphaChar))
    )
}
