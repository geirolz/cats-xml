package cats.xml.scala.xml.codec

import cats.xml.codec.{DataEncoder, Encoder}
import cats.xml.scala.xml.NodeSeqInterop

import scala.annotation.unused
import scala.xml.{Atom, NodeSeq}

private[xml] object NodeSeqEncoder {

  def apply[T](f: T => NodeSeq): Encoder[T] =
    Encoder.of(t => NodeSeqInterop.fromNodeSeq(f(t)))
}

private[xml] trait NodeSeqEncoderSyntax {

  implicit class NodeSeqEncoderObjOps(@unused encoderObj: Encoder.type) {
    def ofNodeSeq[T](f: T => NodeSeq): Encoder[T] =
      NodeSeqEncoder(f)
  }
}

private[xml] trait NodeSeqEncoderInstances {
  implicit val encoderStdAtomStr: DataEncoder[Atom[String]] =
    DataEncoder.encoderString.contramap(_.data.trim)
}
