package io.chrisdavenport.circecoproduct

import io.circe.Decoder
import io.circe._
import io.circe.generic.semiauto._
// import shapeless.{Coproduct, :+:, CNil, Inl, Inr, Lazy}
import shapeless._
import shapeless.ops.coproduct._
// import shapeless.ops.coproduct.Inject

object Decode {

  case class Foo(x: String)
  object Foo {
    implicit val fooDecoder: Decoder[Foo] = Decoder.instance{h =>
      Decoder.resultInstance.map(h.downField("x").as[String])(Foo(_))
    }
  }
  case class Bar(y: String)
  object Bar {
    implicit val barDecoder: Decoder[Bar] = Decoder.instance{h =>
      Decoder.resultInstance.map(h.downField("y").as[String])(Bar(_))
    }
  }
  implicit val cnilDecoder : Decoder[CNil] = 
    Decoder.failed[CNil](DecodingFailure("Failed to Decode Coproduct CNil", List.empty))

  val decoderCoproduct : Decoder[Foo :+: Bar :+: CNil] = Decoder.instance[Foo :+: Bar :+: CNil]{
      hcursor => 
      Decoder[Foo].map(f => Coproduct[Foo :+: Bar :+: CNil](f))
      .or(Decoder[Bar].map(b => Coproduct[Foo :+: Bar :+: CNil](b)))
      .or(Decoder[CNil].map(cnil => Coproduct[Foo :+: Bar :+: CNil](???)))(hcursor)
  }

  def decodeCoproduct[H, T <: Coproduct](
    implicit HDecoder: Lazy[Decoder[H]], 
    TDecoder: Decoder[T],
    HMe: ToHList[T]
    ): Decoder[H :+: T] = {
      val coproductMapper: T => H :+: T = ???


      Decoder.instance[H :+: T]{hlist => 
        HDecoder.value.map(h => Coproduct[H :+: T](h))
          .or(TDecoder.map(coproductMapper))(hlist)
      }
    }

  // def decodeOut[C](p: Poly[HCursor, C <: Coproduct])



  // val coproductDecoder : Decoder[Foo :+: Bar :+: CNil] = 
  implicit def coproductDecoder[C <: Coproduct](implicit HMe: ToHList[C]): Decoder[C] = {
    ??? 
  }


}