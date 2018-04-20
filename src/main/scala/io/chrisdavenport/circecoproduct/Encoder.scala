package io.chrisdavenport.circecoproduct

import io.circe._
import shapeless._

object Enc {
  // Impossible
  implicit val cnilEncoder: Encoder[CNil] = Encoder.instance[CNil](_ => ???)
  
  implicit def coproductEncoder[H, T <: Coproduct](
    implicit hEncoder: Lazy[Encoder[H]],
    tEncoder: Encoder[T]
  ): Encoder[H :+: T] =
    Encoder.instance[H :+: T]{
      case Inl(h) => hEncoder.value(h)
      case Inr(t) => tEncoder(t)
    }

  object Test {
    case class Foo(x: String)
    object Foo {
      implicit val fooEnc = Encoder.instance[Foo]{f => 
        Json.obj(
          "x" -> Json.fromString(f.x)
        )
      }
    }
    case class Bar(y: String)
    object Bar {
      implicit val barEnc = Encoder.instance[Bar]{b => 
      Json.obj(
        "y" -> Json.fromString(b.y)
      )}
    }

    val coproduct : Encoder[Foo :+: Bar :+: CNil] = Encoder[Foo :+: Bar :+: CNil]
  }
}