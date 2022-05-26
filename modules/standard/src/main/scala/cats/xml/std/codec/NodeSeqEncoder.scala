package cats.xml.std.codec

import cats.xml.codec.{DataEncoder, Encoder}
import cats.xml.std.NodeSeqConverter

import scala.annotation.unused
import scala.xml.{Atom, NodeSeq}

private[xml] object NodeSeqEncoder extends NodeSeqEncoderInstances with NodeSeqEncoderSyntax {

  def apply[T](f: T => NodeSeq): Encoder[T] =
    Encoder.of(t => NodeSeqConverter.fromNodeSeq(f(t)))
}
private[xml] trait NodeSeqEncoderInstances {
  implicit val encoderStdAtomStr: DataEncoder[Atom[String]] =
    Encoder.encoderString.contramap(_.data.trim)
}

private[xml] trait NodeSeqEncoderSyntax {

  implicit class NodeSeqEncoderObjOps(@unused encoderObj: Encoder.type) {
    def ofNodeSeq[T](f: T => NodeSeq): Encoder[T] =
      NodeSeqEncoder(f)
  }
}
