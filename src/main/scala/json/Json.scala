package json

import cats.syntax.all.*
import io.circe.{Decoder, Encoder, Codec, Json}
import io.circe
import scala.annotation.targetName

case class Fix[F[_]](unfix: F[Fix[F]])
object Fix:
  given [F[_]](using e: => Encoder[F[Fix[F]]]): Encoder[Fix[F]] = e.contramap(_.unfix)
  given [F[_]](using d: => Decoder[F[Fix[F]]]): Decoder[Fix[F]] = d.map(Fix(_))

case object JsonNull:
  given Encoder[JsonNull] = _ => Json.Null
  given Decoder[JsonNull] = Decoder[Json].emap {
    case Json.Null => Right(JsonNull)
    case _         => Left("expected null")
  }

type JsonNull = JsonNull.type

case class JsonArray[A](elements: List[A])

case class JsonMember[A](value: A)
object JsonMember:
  given nonOpt[K, V: Decoder](using
      f: JsonFieldCodec[K]
  ): Decoder[JsonMember[(K, V)]] =
    Decoder[V].at(f.encode).map(v => JsonMember(f.decode -> v))

  given opt[K, V: Decoder](using
      f: JsonFieldCodec[K]
  ): Decoder[JsonMember[Option[(K, V)]]] = _.downField(f.encode).success
    .traverse(_.as[V].map(f.decode -> _))
    .map(JsonMember(_))

object JsonArray {
  given [A: Encoder]: Encoder[JsonArray[A]] =
    Encoder[List[A]].contramap(_.elements)
  given [A: Decoder]: Decoder[JsonArray[A]] = Decoder[List[A]].map(JsonArray(_))
}

trait JsonFieldCodec[A]:
  def encode: String
  def decode: A

object JsonFieldCodec:
  given [A <: String: ValueOf]: JsonFieldCodec[A] with
    def encode: String = summon[ValueOf[A]].value
    def decode: A = summon[ValueOf[A]].value

case class JsonObject[A](pairs: A)

object JsonObject:
  given [A: JsonMembersEncoder]: Encoder[JsonObject[A]] = a =>
    Json.fromFields(summon[JsonMembersEncoder[A]].encode(a.pairs))
  given Decoder[JsonObject[EmptyTuple]] =
    Decoder[circe.JsonObject].as(JsonObject(EmptyTuple))
  given [A, T <: Tuple](using
      Decoder[JsonMember[A]],
      Decoder[JsonObject[T]]
  ): Decoder[JsonObject[(A *: T)]] =
    (Decoder[JsonMember[A]], Decoder[JsonObject[T]]).mapN {
      case (JsonMember(a), JsonObject(t)) => JsonObject(a *: t)
    }
  type Solo[A] = JsonObject[A *: EmptyTuple]
  def getSoloValue[K, V]: Solo[(K, V)] => V = _.pairs.head._2
  def empty: JsonObject[EmptyTuple] = JsonObject(EmptyTuple)

trait JsonMembersEncoder[A]:
  def encode(a: A): List[(String, Json)]

object JsonMembersEncoder:
  given JsonMembersEncoder[EmptyTuple] = _ => List.empty
  given [K: JsonFieldCodec, V: Encoder, T <: Tuple: JsonMembersEncoder]
      : JsonMembersEncoder[(K, V) *: T] = { case (key, value) *: tail =>
    (summon[JsonFieldCodec[K]].encode, Encoder[V].apply(value)) :: summon[
      JsonMembersEncoder[T]
    ].encode(tail)
  }

type /:[L, R] = Either[L, R]

given [L: Encoder, R: Encoder]: Encoder[L /: R] = {
  case Left(l)  => Encoder[L].apply(l)
  case Right(r) => Encoder[R].apply(r)
}

given [L: Decoder, R: Decoder]: Decoder[L /: R] =
  Decoder[L].map(Left(_)).or(Decoder[R].map(Right(_)))

type SoloObj[A] = JsonObject[A *: EmptyTuple]

trait Field(value: String)

type Nullable[A] = Either[JsonNull, A]

object Nullable:
  def fromOption[A]: Option[A] => Nullable[A] = _.fold(Left(JsonNull))(Right(_))

def getNullable[A]: Nullable[A] => Option[A] =
  _.fold[Option[A]](_ => None, Some(_))
def getOptionalNullable[A, B]: Option[(A, Nullable[B])] => Option[(A, B)] =
  _.flatMap { case (a, nullableB) =>
    getNullable(nullableB).map(a -> _)
  }

def oNValue[A]: [B] => Option[(A, Nullable[B])] => Option[B] = [B] =>
  (x: Option[(A, Nullable[B])]) => getOptionalNullable(x).map(_._2)
