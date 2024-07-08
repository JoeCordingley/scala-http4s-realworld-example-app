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

type JsonSchemaCodec = json.Fix[JsonSchemaCodec.Unfixed]
case class JsonSchema(schemas: List[JsonSchema.Singular])

object JsonSchema:
  def or(left: JsonSchema, right: JsonSchema): JsonSchema = JsonSchema(
    left.schemas ++ right.schemas
  )

  enum Singular:
    case String(
        format: Option[java.lang.String],
        minLength: Option[Int],
        maxLength: Option[Int]
    )
    case Null
    case Integer
    case Object(
        properties: Option[Map[java.lang.String, JsonSchema]],
        required: Option[List[java.lang.String]],
        additionalProperties: Option[JsonSchema]
    )
    case Boolean
    case Number
    case Array(items: JsonSchema)
    case True

  def string(
      format: Option[String] = None,
      minLength: Option[Int] = None,
      maxLength: Option[Int] = None
  ): JsonSchema =
    JsonSchema.fromSingular(
      JsonSchema.Singular.String(format, minLength, maxLength)
    )
  val integer: JsonSchema =
    JsonSchema.fromSingular(JsonSchema.Singular.Integer)
  val boolean: JsonSchema =
    JsonSchema.fromSingular(JsonSchema.Singular.Boolean)
  val `null`: JsonSchema =
    JsonSchema.fromSingular(JsonSchema.Singular.Null)
  def `object`(
      properties: Option[Map[String, JsonSchema]] = None,
      required: Option[List[String]] = None,
      additionalProperties: Option[JsonSchema] = None
  ): JsonSchema =
    JsonSchema.fromSingular(
      JsonSchema.Singular.Object(
        properties,
        required,
        additionalProperties
      )
    )
  val `true`: JsonSchema =
    JsonSchema.fromSingular(JsonSchema.Singular.True)
  val `number`: JsonSchema =
    JsonSchema.fromSingular(JsonSchema.Singular.Number)
  def array(items: JsonSchema): JsonSchema =
    JsonSchema.fromSingular(JsonSchema.Singular.Array(items))

  def fromSingular(schema: JsonSchema.Singular): JsonSchema = JsonSchema(
    List(schema)
  )

object JsonSchemaCodec:
  val `true`: JsonSchemaCodec = Fix(Left(true))
  def `object`(
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

  def simplyTyped(s: SchemaType): JsonSchemaCodec = JsonSchemaCodec.`object`(
    `type` = Some(Left(s))
  )

  def fromSingular(
      overrideType: Option[Option[Either[SchemaType, JsonArray[SchemaType]]]] =
        None
  ): JsonSchema.Singular => JsonSchemaCodec = {
    case JsonSchema.Singular.String(None, None, None)
        if overrideType.exists(
          _.isEmpty
        ) =>
      JsonSchemaCodec.`true`
    case JsonSchema.Singular.String(format, minLength, maxLength) =>
      JsonSchemaCodec.`object`(
        `type` = overrideType.getOrElse(Some(Left(SchemaType.String))),
        format = format,
        minLength = minLength,
        maxLength = maxLength
      )
    case JsonSchema.Singular.True    => JsonSchemaCodec.`true`
    case JsonSchema.Singular.Null    => simplyTyped(SchemaType.Null)
    case JsonSchema.Singular.Integer => simplyTyped(SchemaType.Integer)
    case JsonSchema.Singular.Object(
          properties,
          required,
          additionalProperties
        ) =>
      JsonSchemaCodec.`object`(
        `type` = overrideType.getOrElse(Some(Left(SchemaType.Object))),
        properties = properties.map(properties =>
          JsonObject(properties.view.mapValues(fromJsonSchema).toMap)
        ),
        required = required.map(JsonArray(_)),
        additionalProperties = additionalProperties.map(fromJsonSchema)
      )
    case JsonSchema.Singular.Boolean =>
      simplyTyped(SchemaType.Boolean)
    case JsonSchema.Singular.Number =>
      simplyTyped(SchemaType.Number)
    case JsonSchema.Singular.Array(
          JsonSchema(List(JsonSchema.Singular.True))
        ) =>
      simplyTyped(SchemaType.Array)
    case JsonSchema.Singular.Array(
          items
        ) =>
      JsonSchemaCodec.`object`(
        `type` = Some(Left(SchemaType.Array)),
        items = Some(fromJsonSchema(items))
      )
  }

  def fromJsonSchema: JsonSchema => JsonSchemaCodec = {
    case JsonSchema(schemas) =>
      schemas match {
        case List(JsonSchema.Singular.True) => JsonSchemaCodec.`true`
        case List(schema)                   => fromSingular(None)(schema)
        case schemas =>
          JsonSchemaCodec.`object`(
            anyOf = Some(JsonArray(schemas.map(fromSingular())))
          )
      }
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
  given objMap[A: SchemaOf]: SchemaOf[JsonObject[Map[String, A]]] with
    def apply: JsonSchema = JsonSchema.`object`(
      additionalProperties = Some(summon[SchemaOf[A]].apply)
    )
  given SchemaOf[String] with
    def apply: JsonSchema = JsonSchema.string()
  given SchemaOf[Int] with
    def apply: JsonSchema = JsonSchema.integer
  given SchemaOf[Boolean] with
    def apply: JsonSchema = JsonSchema.boolean
  given SchemaOf[JsonNull] with
    def apply: JsonSchema = JsonSchema.`null`
  given SchemaOf[circe.JsonObject] with
    def apply: JsonSchema =
      JsonSchema.`object`()
  given SchemaOf[circe.Json] with
    def apply: JsonSchema = JsonSchema.`true`
  given SchemaOf[Double] with
    def apply: JsonSchema = JsonSchema.number
  given SchemaOf[Email] with
    def apply: JsonSchema = JsonSchema.string(format = Some("email"))
  given [A: SchemaOf]: SchemaOf[JsonArray[A]] with
    def apply: JsonSchema = JsonSchema.array(summon[SchemaOf[A]].apply)
  given objWithProperties[A: PropertiesOf: RequiredOf]: SchemaOf[JsonObject[A]]
  with
    def apply: JsonSchema = JsonSchema.`object`(
      properties = Some(summon[PropertiesOf[A]].apply),
      required = Some(summon[RequiredOf[A]].apply)
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
