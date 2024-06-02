package json

import org.http4s.Uri
import cats.syntax.option

enum SchemaType:
  case String
  case Object
  case Integer
  case Boolean
  case Null
  case Array
  case Number

type JsonSchema = json.Fix[[A] =>> JsonObject[
  (
      Option[("type", SchemaType)],
      Option[("properties", JsonObject[Map[String, A]])],
      Option[("required", JsonArray[String])],
      Option[("items", A)],
      Option[("additionalProperties", A)]
  )
]]
object JsonSchema:
  def apply(
      `type`: Option[SchemaType] = None,
      properties: Option[Map[String, JsonSchema]] = None,
      required: Option[List[String]] = None,
      items: Option[JsonSchema] = None,
      additionalProperties: Option[JsonSchema] = None
  ): JsonSchema = Fix(
    JsonObject(
      (
        `type`.map("type" -> _),
        properties
          .map(values => "properties" -> JsonObject(values)),
        required.map(values => "required" -> JsonArray(values)),
        items.map("items" -> _),
        additionalProperties.map(
          "additionalProperties" -> _
        ),
      )
    )
  )
trait SchemaOf[A]:
  def apply: JsonSchema

object SchemaOf:
  given SchemaOf[String] with
    def apply: JsonSchema = JsonSchema(`type` = Some(SchemaType.String))
  given SchemaOf[Int] with
    def apply: JsonSchema = JsonSchema(`type` = Some(SchemaType.Integer))
  given SchemaOf[Boolean] with
    def apply: JsonSchema = JsonSchema(`type` = Some(SchemaType.Boolean))
  given SchemaOf[JsonNull] with
    def apply: JsonSchema = JsonSchema(`type` = Some(SchemaType.Null))
  given SchemaOf[Double] with
    def apply: JsonSchema = JsonSchema(`type` = Some(SchemaType.Number))
  given [A: SchemaOf]: SchemaOf[JsonArray[A]] with
    def apply: JsonSchema = JsonSchema(
      `type` = Some(SchemaType.Array),
      items = Some(summon[SchemaOf[A]].apply)
    )
  given objWithProperties[A: PropertiesOf: RequiredOf]: SchemaOf[JsonObject[A]]
  with
    def apply: JsonSchema =
      JsonSchema(
        `type` = Some(SchemaType.Object),
        properties = Some(summon[PropertiesOf[A]].apply),
        required = Some(summon[RequiredOf[A]].apply)
      )
  given objMap[A: SchemaOf]: SchemaOf[JsonObject[Map[String, A]]] with
    def apply: JsonSchema = JsonSchema(
      `type` = Some(SchemaType.Object),
      additionalProperties = Some(summon[SchemaOf[A]].apply)
    )

trait PropertiesOf[A]:
  def apply: Map[String, JsonSchema]
object PropertiesOf:
  given PropertiesOf[EmptyTuple] with
    def apply: Map[String, JsonSchema] = Map.empty
  given nonOpt[A: JsonFieldCodec, B: SchemaOf, C <: Tuple: PropertiesOf]
      : PropertiesOf[(A, B) *: C] with
    def apply: Map[String, JsonSchema] = summon[
      PropertiesOf[C]
    ].apply + (summon[JsonFieldCodec[A]].encode -> summon[SchemaOf[B]].apply)
  given opt[A: JsonFieldCodec, B: SchemaOf, C <: Tuple: PropertiesOf]
      : PropertiesOf[Option[(A, B)] *: C] with
    def apply: Map[String, JsonSchema] = nonOpt[A, B, C].apply

trait RequiredOf[A]:
  def apply: List[String]

object RequiredOf:
  given RequiredOf[EmptyTuple] with
    def apply: List[String] = List.empty
  given nonOpt[A: JsonFieldCodec, B, C <: Tuple: RequiredOf]
      : RequiredOf[(A, B) *: C] with
    def apply: List[String] =
      summon[JsonFieldCodec[A]].encode :: summon[RequiredOf[C]].apply
  given opt[A, C <: Tuple: RequiredOf]: RequiredOf[Option[A] *: C] with
    def apply: List[String] = summon[RequiredOf[C]].apply
