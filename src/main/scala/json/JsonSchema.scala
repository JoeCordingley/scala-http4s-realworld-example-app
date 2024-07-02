package json

import org.http4s.Uri
import cats.syntax.option
import io.circe.Encoder
import io.circe
import cats.syntax.all.*

type SchemaType = String

object SchemaType:
  val String = "string"
  val Object = "object"
  val Integer = "integer"
  val Boolean = "boolean"
  val Null = "null"
  val Array = "array"
  val Number = "number"
  def fromSingular: JsonSchema.Singular => Option[SchemaType] = {
    case JsonSchema.Singular.String(_)    => Some(SchemaType.String)
    case JsonSchema.Singular.Null         => Some(SchemaType.Null)
    case JsonSchema.Singular.Integer      => Some(SchemaType.Integer)
    case JsonSchema.Singular.Object(_, _) => Some(SchemaType.Object)
    case JsonSchema.Singular.Boolean      => Some(SchemaType.Boolean)
    case JsonSchema.Singular.Number       => Some(SchemaType.Number)
    case JsonSchema.Singular.True         => None
  }

type JsonSchemaCodec = json.Fix[JsonSchemaCodec.Unfixed]
case class JsonSchema(schemas: List[JsonSchema.Singular])

object JsonSchema:
  def or(left: JsonSchema, right: JsonSchema): JsonSchema = JsonSchema(
    left.schemas ++ right.schemas
  )

  enum Singular:
    case String(format: Option[java.lang.String] = None)
    case Null
    case Integer
    case Object(
        properties: Option[Map[java.lang.String, JsonSchema]] = None,
        required: Option[List[java.lang.String]] = None
    )
    case Boolean
    case Number
    case True
  object Singular:
    def isComplex: Singular => Boolean = {
      case String(format) => format.isDefined
      case Null           => false
      case Integer        => false
      case Object(properties, required) =>
        properties.isDefined || required.isDefined
      case Boolean => false
      case Number =>
        false
      case True => false
    }

  def fromSingular(schema: JsonSchema.Singular): JsonSchema = JsonSchema(
    List(schema)
  )

object JsonSchemaCodec:
  def apply(
      `type`: Option[Either[SchemaType, JsonArray[SchemaType]]] = None,
      properties: Option[JsonObject[Map[String, JsonSchemaCodec]]] = None,
      required: Option[JsonArray[String]] = None,
      items: Option[JsonSchemaCodec] = None,
      additionalProperties: Option[JsonSchemaCodec] = None,
      format: Option[String] = None,
      minLength: Option[Int] = None,
      maxLength: Option[Int] = None,
      anyOf: Option[JsonArray[JsonSchemaCodec]] = None
  ): JsonSchemaCodec = Fix(
    Right(
      JsonObject(
        (
          `type`.map("type" -> _),
          properties.map("properties" -> _),
          required.map("required" -> _),
          items.map("items" -> _),
          additionalProperties.map(
            "additionalProperties" -> _
          ),
          format.map("format" -> _),
          minLength.map("minLength" -> _),
          maxLength.map("maxLength" -> _),
          anyOf.map("anyOf" -> _),
        )
      )
    )
  )

  def fromJsonSchema: JsonSchema => JsonSchemaCodec = {
    case JsonSchema(List(JsonSchema.Singular.True)) => Fix(Left(true))
    case JsonSchema(schemas) =>
      val (maybeProperties, maybeRequired) =
        schemas.filter(JsonSchema.Singular.isComplex) match {
          case List(JsonSchema.Singular.Object(properties, required)) =>
            (properties, required)
          case _ => (None, None)
        }
      JsonSchemaCodec.apply(
        `type` = Some(
          singular(schemas.flatMap(SchemaType.fromSingular))
            .map(JsonArray(_))
        ),
        properties = maybeProperties.map(properties =>
          JsonObject(properties.view.mapValues(fromJsonSchema).toMap)
        ),
        required = maybeRequired.map(JsonArray(_))
      )

  }
  def of[A: SchemaOf]: JsonSchemaCodec =
    JsonSchemaCodec.fromJsonSchema(summon[SchemaOf[A]].apply)

  given encoder(using
      e: => Encoder[Unfixed[Fix[Unfixed]]]
  ): Encoder[JsonSchemaCodec] = e.contramap(_.unfix)
  type Unfixed[A] = Either[
    Boolean,
    JsonObject[
      (
          Option[("type", Either[SchemaType, JsonArray[SchemaType]])],
          Option[("properties", JsonObject[Map[String, A]])],
          Option[("required", JsonArray[String])],
          Option[("items", A)],
          Option[("additionalProperties", A)],
          Option[("format", String)],
          Option[("minLength", Int)],
          Option[("maxLength", Int)],
          Option[("anyOf", JsonArray[A])]
      )
    ]
  ]

  def singular[A]: List[A] => Either[A, List[A]] = {
    case List(x) => Left(x)
    case xs      => Right(xs)
  }

trait SchemaOf[A]:
  def apply: JsonSchema

object SchemaOf:
//  given objMap[A: SchemaOf]: SchemaOf[JsonObject[Map[String, A]]] with
//    def apply: JsonSchemaCodec = JsonSchemaCodec(
//      `type` = Some(Left(SchemaType.Object)),
//      additionalProperties = Some(summon[SchemaOf[A]].apply)
//    )
  given SchemaOf[String] with
    def apply: JsonSchema =
      JsonSchema.fromSingular(JsonSchema.Singular.String())
  given SchemaOf[Int] with
    def apply: JsonSchema = JsonSchema.fromSingular(JsonSchema.Singular.Integer)
  given SchemaOf[Boolean] with
    def apply: JsonSchema = JsonSchema.fromSingular(JsonSchema.Singular.Boolean)
  given SchemaOf[JsonNull] with
    def apply: JsonSchema = JsonSchema.fromSingular(JsonSchema.Singular.Null)
  given SchemaOf[circe.JsonObject] with
    def apply: JsonSchema =
      JsonSchema.fromSingular(JsonSchema.Singular.Object())
  given SchemaOf[circe.Json] with
    def apply: JsonSchema = JsonSchema.fromSingular(JsonSchema.Singular.True)
  given SchemaOf[Double] with
    def apply: JsonSchema = JsonSchema.fromSingular(JsonSchema.Singular.Number)
  given SchemaOf[Email] with
    def apply: JsonSchema =
      JsonSchema.fromSingular(
        JsonSchema.Singular.String(format = Some("email"))
      )
//  given [A: SchemaOf]: SchemaOf[JsonArray[A]] with
//    def apply: JsonSchemaCodec = JsonSchemaCodec(
//      `type` = Some(Left(SchemaType.Array)),
//      items = Some(summon[SchemaOf[A]].apply)
//    )
  given objWithProperties[A: PropertiesOf: RequiredOf]: SchemaOf[JsonObject[A]]
  with
    def apply: JsonSchema =
      JsonSchema.fromSingular(
        JsonSchema.Singular.Object(
          properties = Some(summon[PropertiesOf[A]].apply),
          required = Some(summon[RequiredOf[A]].apply)
        )
      )
  given [A: SchemaOf, B: SchemaOf]: SchemaOf[Either[A, B]] with
    def apply: JsonSchema =
      JsonSchema.or(summon[SchemaOf[A]].apply, summon[SchemaOf[B]].apply)

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
