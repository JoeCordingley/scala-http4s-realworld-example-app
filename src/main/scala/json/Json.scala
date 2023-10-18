package json

import cats.syntax.all.*
import io.circe.{Decoder, Encoder, Json}
import io.circe

case object JsonNull:
  given Encoder[JsonNull] = _ => Json.Null
  given Decoder[JsonNull] = Decoder[Json].emap{
    case Json.Null => Right(JsonNull)
    case _ => Left("expected null")
  }

type JsonNull = JsonNull.type

case class JsonArray[A](elements: List[A])

case class JsonMember[A](value: A)
object JsonMember:
  given [K, V: Decoder](using f: JsonFieldEncoder[K]): Decoder[JsonMember[(K, V)]] = Decoder[V].at(f.encode).map(v => JsonMember(f.decode -> v))

object JsonArray {
  given [A: Encoder]: Encoder[JsonArray[A]] = Encoder[List[A]].contramap(_.elements)
  given [A: Decoder]: Decoder[JsonArray[A]] = Decoder[List[A]].map(JsonArray(_))
}


trait JsonFieldEncoder[A]:
  def encode: String
  def decode: A

case class JsonObject[A](pairs: A)

object JsonObject:
  given [A: JsonMembersEncoder]: Encoder[JsonObject[A]] = a => Json.fromFields(summon[JsonMembersEncoder[A]].encode(a.pairs))
  given Decoder[JsonObject[EmptyTuple]] = Decoder[circe.JsonObject].as(JsonObject(EmptyTuple))
  given [A, T <: Tuple](using Decoder[JsonMember[A]], Decoder[JsonObject[T]]): Decoder[JsonObject[(A *: T)]] = 
    (Decoder[JsonMember[A]], Decoder[JsonObject[T]]).mapN{
      case (JsonMember(a), JsonObject(t)) => JsonObject(a *: t)
    }

trait JsonMembersEncoder[A]:
  def encode(a: A): List[(String, Json)]

object JsonMembersEncoder:
  given JsonMembersEncoder[EmptyTuple] = _ => List.empty
  given [K: JsonFieldEncoder, V: Encoder, T <: Tuple: JsonMembersEncoder]: JsonMembersEncoder[(K, V) *: T] = {
    case (key, value) *: tail => (summon[JsonFieldEncoder[K]].encode, Encoder[V].apply(value)) :: summon[JsonMembersEncoder[T]].encode(tail)
  }


given [A <: String: ValueOf]: JsonFieldEncoder[A] with
  def encode: String = summon[ValueOf[A]].value
  def decode: A = summon[ValueOf[A]].value

//trait JsonMemberDecoder[A]:
//  def decoder: Decoder[A]
//
//object JsonMemberDecoder:
//  given JsonMemberDecoder[EmptyTuple] with
//    def decoder: Decoder[EmptyTuple] = Decoder
////  given [A: Decoder, T <: Tuple: JsonMemberDecoder]: JsonMemberDecoder[A *: T] = new JsonMemberDecoder[A]:
////    def decoder: Decoder[A] = 
////    (JsonMemberDecoder[A]].decoder(map), summon[JsonMemberDecoder[T]].decode(map)).mapN(_ *: _)
////
////  given [K: JsonFieldEncoder, V: Decoder]: JsonMemberDecoder[(K, V)] = map => for {
////    j <- map.get(summon[JsonFieldEncoder[K]].encode).toRight("")
////    v <- Decoder[V].decodeJson(j).leftMap(_.message)
////  } yield (summon[JsonFieldEncoder[K]].decode, v)


