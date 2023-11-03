package json

import org.http4s.Uri
import java.lang.{String as SString}

//case class JsonSchema[A](`$schema`: Option[Uri] = None, `$id`: Option[Uri] = None, title: Option[String] = None, description: Option[String] = None, schemaType: SchemaType[A])

enum JsonSchema[A]:
  case String extends JsonSchema[SString]
  case Int extends JsonSchema[Integer]
  case Array[A](schema: JsonSchema[A]) extends JsonSchema[JsonArray[A]]
  case Object[A](properties: JsonProperties[A]) extends JsonSchema[JsonObject[A]]

enum JsonProperties[A]:
  case Empty extends JsonProperties[EmptyTuple]
  case Cons[A, B, C <: Tuple](head: (A, JsonSchema[B]), tail: JsonProperties[C]) extends JsonProperties[(A, B) *: C]

val x: JsonSchema[JsonArray[JsonObject[(("id", String), ("name" , String))]]] = JsonSchema.Array(JsonSchema.Object(JsonProperties.Cons(("id", JsonSchema.String), JsonProperties.Cons(("name", JsonSchema.String), JsonProperties.Empty))))
