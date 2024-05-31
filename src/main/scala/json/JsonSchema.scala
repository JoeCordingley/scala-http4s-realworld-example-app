package json

import org.http4s.Uri

//case class JsonSchema[A](`$schema`: Option[Uri] = None, `$id`: Option[Uri] = None, title: Option[String] = None, description: Option[String] = None, schemaType: SchemaType[A])

enum SchemaType:
  case String
  case Object
  case Int
  case Boolean
  case Null
  case Array

type JsonSchemaCodec = JsonObject[(Option[("type", SchemaType)], Option[("properties", JsonObject[Map[String, JsonSchemaCodec]])])]

enum JsonSchema[A]:
  case String extends JsonSchema[java.lang.String]
  case Int extends JsonSchema[Int]
  case Boolean extends JsonSchema[Boolean]
  case Null extends JsonSchema[JsonNull]
  case Array[A](schema: JsonSchema[A]) extends JsonSchema[JsonArray[A]]
  case Object[A](properties: JsonProperties[A])
      extends JsonSchema[JsonObject[A]]
//object JsonSchema:
//  def obj[A, B](a: A)(using t: ToProperties[A, B]): JsonSchema[JsonObject[B]] =
//    Object(t.apply(a))

//trait ToProperties[A, B]:
//  def apply(properties: A): JsonProperties[B]
//
//object ToProperties:
//  given ToProperties[EmptyTuple, EmptyTuple] = _ => JsonProperties.Empty
//  given [A, B, S <: Tuple, T <: Tuple](using
//      t: ToProperties[S, T]
//  ): ToProperties[(A, JsonSchema[B]) *: S, (A, B) *: T] = { case pair *: s =>
//    JsonProperties.Cons(pair, t.apply(s))
//  }
//  def toProperties[A, B](a: A)(using t: ToProperties[A, B]): JsonProperties[B] =
//    t.apply(a)
//
//extension [A, B](t: (A, JsonSchema[B]))
//  def **:[C <: Tuple](c: JsonProperties[C]): JsonProperties[(A, B) *: C] =
//    JsonProperties.Cons(t, c)

enum JsonProperties[A]:
  case Empty extends JsonProperties[EmptyTuple]
  case Cons[A, B, C <: Tuple](head: (A, JsonSchema[B]), tail: JsonProperties[C])
      extends JsonProperties[(A, B) *: C]


//val y: JsonProperties[(("id", String), ("name", String))] =
//    ToProperties.toProperties((("id": "id"), JsonSchema.String), (("name": "name"), JsonSchema.String))

val x: JsonSchema[JsonArray[JsonObject[(("id", String), ("name", String))]]] =
  JsonSchema.Array(
    JsonSchema.Object(
      JsonProperties.Cons(
        ("id", JsonSchema.String),
        JsonProperties.Cons(("name", JsonSchema.String), JsonProperties.Empty)
      )
    )
  )

trait DefaultSchema[A]:
  def apply: JsonSchema[A]

object DefaultSchema:
  given DefaultSchema[String] with
    def apply: JsonSchema[String] = JsonSchema.String
  given DefaultSchema[Int] with
    def apply: JsonSchema[Int] = JsonSchema.Int
  given DefaultSchema[Boolean] with
    def apply: JsonSchema[Boolean] = JsonSchema.Boolean
  given [A: DefaultSchema]: DefaultSchema[JsonArray[A]] with
    def apply: JsonSchema[JsonArray[A]] = JsonSchema.Array(summon[DefaultSchema[A]].apply)
  given [A: DefaultProperties]: DefaultSchema[JsonObject[A]] with 
    def apply: JsonSchema[JsonObject[A]] = JsonSchema.Object(summon[DefaultProperties[A]].apply)

trait DefaultProperties[A]:
  def apply: JsonProperties[A]

object DefaultProperties:
  given DefaultProperties[EmptyTuple] with
    def apply: JsonProperties[EmptyTuple] = JsonProperties.Empty
  given [A: ValueOf, B: DefaultSchema, C <: Tuple: DefaultProperties]: DefaultProperties[(A, B) *: C] with
    def apply: JsonProperties[(A,B) *: C] = JsonProperties.Cons((summon[ValueOf[A]].value, summon[DefaultSchema[B]].apply), summon[DefaultProperties[C]].apply)


