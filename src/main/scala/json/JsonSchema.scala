package json

import org.http4s.Uri
import io.circe.Encoder

//case class JsonSchema[A](`$schema`: Option[Uri] = None, `$id`: Option[Uri] = None, title: Option[String] = None, description: Option[String] = None, schemaType: SchemaType[A])

enum JsonSchema[A]:
  case String extends JsonSchema[java.lang.String]
  case Int extends JsonSchema[Integer]
  case Array[A](schema: JsonSchema[A]) extends JsonSchema[JsonArray[A]]
  case Object[A](properties: JsonProperties[A])
      extends JsonSchema[JsonObject[A]]
object JsonSchema:
  def obj[A, B](a: A)(using t: ToProperties[A, B]): JsonSchema[JsonObject[B]] =
    Object(t.apply(a))

trait ToProperties[A, B]:
  def apply(properties: A): JsonProperties[B]

object ToProperties:
  given ToProperties[EmptyTuple, EmptyTuple] = _ => JsonProperties.Empty
  given [A, B, S <: Tuple, T <: Tuple](
    using t: ToProperties[S, T]
  ): ToProperties[(A, JsonSchema[B]) *: S, (A, B) *: T] = { case pair *: s =>
    JsonProperties.Cons(pair, t.apply(s))
  }

enum JsonProperties[A]:
  case Empty extends JsonProperties[EmptyTuple]
  case Cons[A, B, C <: Tuple](head: (A, JsonSchema[B]), tail: JsonProperties[C])
      extends JsonProperties[(A, B) *: C]

extension [A, B](t: (A, JsonSchema[B]))
  def **:[C <: Tuple](c: JsonProperties[C]): JsonProperties[(A, B)  *: C] = JsonProperties.Cons(t, c)

val y: JsonSchema[JsonArray[JsonObject[(("id", String), ("name", String))]]] =
  JsonSchema.Array(
    JsonSchema.obj[
      (("id", JsonSchema[String]), ("name", JsonSchema[String])),
      (("id", String), ("name", String))
    ](("id", JsonSchema.String), ("name", JsonSchema.String))
  )

val x: JsonSchema[JsonArray[JsonObject[(("id", String), ("name", String))]]] =
  JsonSchema.Array(
    JsonSchema.Object(
      JsonProperties.Cons(
        ("id", JsonSchema.String),
        JsonProperties.Cons(("name", JsonSchema.String), JsonProperties.Empty)
      )
    )
  )

type JsonType = "string" /: "object" /: "array" /: "null" /: "number"
type SchemaType = Either[JsonType, JsonArray[JsonType]]
object SchemaType:
  def single(jsontype: JsonType): SchemaType = Left(jsontype)
  def multiple(jsontypes: JsonType*): SchemaType = Right(JsonArray(jsontypes.toList))

object JsonType:
  val String: JsonType = Left("string")
  val Object: JsonType = Right(Left("object"))
  val Array: JsonType = Right(Right(Left("array")))
  val Null: JsonType = Right(Right(Right(Left("null"))))
  val Number: JsonType = Right(Right(Right(Right("number"))))

type EndSchema = JsonObject.Solo[(
  ("type", SchemaType)
)]

object EndSchema:
  def single(jsonType: JsonType): EndSchema = JsonObject((("type", SchemaType.single(jsonType)) *: EmptyTuple))

  given stringConversion: Conversion[JsonSchema[String], EndSchema] = _ => single(JsonType.String)
  given intConversion: Conversion[JsonSchema[Int], EndSchema] = _ => single(JsonType.Number)
  given doubleConversion: Conversion[JsonSchema[Double], EndSchema] = _ => single(JsonType.Number)
  given nullConversion: Conversion[JsonSchema[Null], EndSchema] = _ => single(JsonType.Null)
  given objectConversion: Conversion[JsonSchema[Null], EndSchema] = _ => single(JsonType.Object)
  given arrayConversion: Conversion[JsonSchema[Null], EndSchema] = _ => single(JsonType.Array)
  

val z = summon[Encoder[EndSchema]]

