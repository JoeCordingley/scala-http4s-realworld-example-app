package json

import org.http4s.Uri
import cats.syntax.option
import io.circe.Encoder

type SchemaType = String

object SchemaType:
  val String = "string"
  val Object = "object"
  val Integer = "integer"
  val Boolean = "boolean"
  val Null = "null"
  val Array = "array"
  val Number = "number"

type JsonSchema = json.Fix[JsonSchema.Unfixed]
object JsonSchema:

  type Unfixed[A] = JsonObject[
    (
        Option[("type", Either[SchemaType, JsonArray[SchemaType]])],
        Option[("properties", JsonObject[Map[String, A]])],
        Option[("required", JsonArray[String])],
        Option[("items", A)],
        Option[("additionalProperties", A)],
        Option[("format", String)],
        Option[("minLength", Int)],
        Option[("maxLength", Int)]
    )
  ]
  def types: JsonSchema => List[SchemaType] = _.unfix match {
    case JsonObject(t) =>
      for {
        (_, either) <- t.head.toList
        types <- either.fold(List(_), _.elements)
      } yield types
  }
  def apply(
      `type`: Option[Either[SchemaType, JsonArray[SchemaType]]] = None,
      properties: Option[Map[String, JsonSchema]] = None,
      required: Option[List[String]] = None,
      items: Option[JsonSchema] = None,
      additionalProperties: Option[JsonSchema] = None,
      format: Option[String] = None,
      minLength: Option[Int] = None,
      maxLength: Option[Int] = None
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
        format.map("format" -> _),
        minLength.map("minLength" -> _),
        maxLength.map("maxLength" -> _),
      )
    )
  )
  given encoder(using
      e: => Encoder[Unfixed[Fix[Unfixed]]]
  ): Encoder[JsonSchema] = e.contramap(_.unfix)
trait SchemaOf[A]:
  def apply: JsonSchema

object SchemaOf:
  given objMap[A: SchemaOf]: SchemaOf[JsonObject[Map[String, A]]] with
    def apply: JsonSchema = JsonSchema(
      `type` = Some(Left(SchemaType.Object)),
      additionalProperties = Some(summon[SchemaOf[A]].apply)
    )
  given SchemaOf[String] with
    def apply: JsonSchema = JsonSchema(`type` = Some(Left(SchemaType.String)))
  given SchemaOf[Int] with
    def apply: JsonSchema = JsonSchema(`type` = Some(Left(SchemaType.Integer)))
  given SchemaOf[Boolean] with
    def apply: JsonSchema = JsonSchema(`type` = Some(Left(SchemaType.Boolean)))
  given SchemaOf[JsonNull] with
    def apply: JsonSchema = JsonSchema(`type` = Some(Left(SchemaType.Null)))
  given SchemaOf[Double] with
    def apply: JsonSchema = JsonSchema(`type` = Some(Left(SchemaType.Number)))
  given SchemaOf[Email] with
    def apply: JsonSchema =
      JsonSchema(`type` = Some(Left(SchemaType.String)), format = Some("email"))
  given [A: SchemaOf]: SchemaOf[JsonArray[A]] with
    def apply: JsonSchema = JsonSchema(
      `type` = Some(Left(SchemaType.Array)),
      items = Some(summon[SchemaOf[A]].apply)
    )
  given objWithProperties[A: PropertiesOf: RequiredOf]: SchemaOf[JsonObject[A]]
  with
    def apply: JsonSchema =
      JsonSchema(
        `type` = Some(Left(SchemaType.Object)),
        properties = Some(summon[PropertiesOf[A]].apply),
        required = Some(summon[RequiredOf[A]].apply)
      )
  given [A: SchemaOf, B: SchemaOf]: SchemaOf[Either[A, B]] with
    def apply: JsonSchema =
      JsonSchema(`type` =
        Some(
          Right(
            JsonArray(
              JsonSchema.types(summon[SchemaOf[A]].apply) ++ JsonSchema.types(
                summon[SchemaOf[B]].apply
              )
            )
          )
        )
      )

trait PropertiesOf[A]:
  def apply: Map[String, JsonSchema]
object PropertiesOf:
  given opt[A: JsonFieldCodec, B: SchemaOf, C <: Tuple: PropertiesOf]
      : PropertiesOf[Option[(A, B)] *: C] with
    def apply: Map[String, JsonSchema] = nonOpt[A, B, C].apply
  given PropertiesOf[EmptyTuple] with
    def apply: Map[String, JsonSchema] = Map.empty
  given nonOpt[A: JsonFieldCodec, B: SchemaOf, C <: Tuple: PropertiesOf]
      : PropertiesOf[(A, B) *: C] with
    def apply: Map[String, JsonSchema] = summon[
      PropertiesOf[C]
    ].apply + (summon[JsonFieldCodec[A]].encode -> summon[SchemaOf[B]].apply)

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
