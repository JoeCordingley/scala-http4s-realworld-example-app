package json

import org.http4s.Uri

//case class JsonSchema[A](`$schema`: Option[Uri] = None, `$id`: Option[Uri] = None, title: Option[String] = None, description: Option[String] = None, schemaType: SchemaType[A])

enum JsonSchema[A]:
  case String extends JsonSchema[java.lang.String]
  case Int extends JsonSchema[Integer]
  case Array[A](schema: JsonSchema[A]) extends JsonSchema[JsonArray[A]]
  case Object[A](properties: JsonProperties[A]) extends JsonSchema[JsonObject[A]]
object JsonSchema:
  def obj[A, B](a: A)(using ToProperties[A, B]): JsonSchema[JsonObject[B]] = Object(summon[ToProperties[A, B]].apply(a))

trait ToProperties[A, B]:
  def apply(properties: A): JsonProperties[B]

object ToProperties:
  given ToProperties[EmptyTuple, EmptyTuple] = _ => JsonProperties.Empty
  given [A, B, S <: Tuple, T <: Tuple](using ToProperties[S, T]): ToProperties[(A, JsonSchema[B]) *: S, (A, B) *: T] = {
    case pair *: s => JsonProperties.Cons(pair, summon[ToProperties[S, T]].apply(s))
  }

enum JsonProperties[A]:
  case Empty extends JsonProperties[EmptyTuple]
  case Cons[A, B, C <: Tuple](head: (A, JsonSchema[B]), tail: JsonProperties[C]) extends JsonProperties[(A, B) *: C]

val x: JsonSchema[JsonArray[JsonObject[(("id", String), ("name" , String))]]] = JsonSchema.Array(JsonSchema.obj[(("id", JsonSchema[String]), ("name" , JsonSchema[String])), (("id", String), ("name" , String))](("id", JsonSchema.String), ("name", JsonSchema.String)))
//val x: JsonSchema[JsonArray[JsonObject[(("id", String), ("name" , String))]]] = JsonSchema.Array(JsonSchema.Object(JsonProperties.Cons(("id", JsonSchema.String), JsonProperties.Cons(("name", JsonSchema.String), JsonProperties.Empty))))
