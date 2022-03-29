package cats.xml.std

import cats.xml.std.testing.NodeSeqAssertions

class XmlNormalizerSuite extends munit.FunSuite with NodeSeqAssertions {

  test("XmlNormalizer.normalize") {
    assertEqualsNodeSeq(
      XmlNormalizer.normalize(
        <Cars>
            <Car V1="1"></Car>
            <Car V1="2">
              <Properties>
                <Property K="1"></Property>
                <Property K="2"></Property>
                <Property K="3">TEXT</Property>
             </Properties>
            </Car>
            <Car V1="4"></Car>
            <Car V1="5">TEXT</Car>
          </Cars>
      ),
      <Cars>
        <Car V1="1"/>
        <Car V1="2">
          <Properties>
            <Property K="1"/>
            <Property K="2"/>
            <Property K="3">TEXT</Property>
          </Properties>
        </Car>
        <Car V1="4"/>
        <Car V1="5">TEXT</Car>
      </Cars>
    )
  }

  test("NodeSeq.normalize") {

    import cats.xml.std.syntax.*

    assertEqualsNodeSeq(
      obtained = <Cars>
        <Car V1="1"></Car>
        <Car V1="2">
          <Properties>
            <Property K="1"></Property>
            <Property K="2"></Property>
            <Property K="3">TEXT</Property>
          </Properties>
        </Car>
        <Car V1="4"></Car>
        <Car V1="5">TEXT</Car>
      </Cars>.normalized,
      expected = <Cars>
        <Car V1="1"/>
        <Car V1="2">
          <Properties>
            <Property K="1"/>
            <Property K="2"/>
            <Property K="3">TEXT</Property>
          </Properties>
        </Car>
        <Car V1="4"/>
        <Car V1="5">TEXT</Car>
      </Cars>
    )
  }

}
