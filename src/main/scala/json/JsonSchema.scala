package json

import org.http4s.Uri
import cats.syntax.option
import io.circe.Encoder
import io.circe
import cats.syntax.all.*
import json.JsonSchema.fromSingular
import json.JsonSchema.ComplexType

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
    case _: JsonSchema.Singular.String => Some(SchemaType.String)
    case JsonSchema.Singular.Null      => Some(SchemaType.Null)
    case JsonSchema.Singular.Integer   => Some(SchemaType.Integer)
    case _: JsonSchema.Singular.Object => Some(SchemaType.Object)
    case JsonSchema.Singular.Boolean   => Some(SchemaType.Boolean)
    case JsonSchema.Singular.Number    => Some(SchemaType.Number)
    case JsonSchema.Singular.True      => None
  }

type JsonSchemaCodec = json.Fix[JsonSchemaCodec.Unfixed]
case class JsonSchema(schemas: List[JsonSchema.Singular])

object JsonSchema:
  def or(left: JsonSchema, right: JsonSchema): JsonSchema = JsonSchema(
    left.schemas ++ right.schemas
  )
  case class ObjectValues(
      properties: Option[Map[java.lang.String, JsonSchema]],
      required: Option[List[java.lang.String]],
      additionalProperties: Option[JsonSchema]
  )

  enum Singular:
    case String(
        format: Option[java.lang.String],
        minLength: Option[Int]
    )
    case Null
    case Integer
    case Object(values: ObjectValues)
    case Boolean
    case Number
    case True

  enum ComplexType:
    case FormattedString(
        format: Option[java.lang.String],
        minLength: Option[Int]
    )
    case Object(values: ObjectValues)
  object ComplexType:
    def fromSingular: Singular => Option[ComplexType] = {
      case Singular.Object(values)
          if values.properties.isDefined || values.required.isDefined || values.additionalProperties.isDefined =>
        Some(Object(values))
      case Singular.String(format, minLength)
          if format.isDefined || minLength.isDefined =>
        Some(FormattedString(format, minLength))
      case _ => None
    }
  def string(
      format: Option[String] = None,
      minLength: Option[Int] = None
  ): JsonSchema =
    JsonSchema.fromSingular(JsonSchema.Singular.String(format, minLength))
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
        ObjectValues(properties, required, additionalProperties)
      )
    )
  val `true`: JsonSchema =
    JsonSchema.fromSingular(JsonSchema.Singular.True)
  val `number`: JsonSchema =
    JsonSchema.fromSingular(JsonSchema.Singular.Number)

  def fromSingular(schema: JsonSchema.Singular): JsonSchema = JsonSchema(
    List(schema)
  )
  def atMostOneComplexTypeAndOneOfEachSimpleType(
      schemas: List[Singular]
  ): (Option[SchemaType], List[SchemaType]) = ???

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
  def encodeProperties(properties: Map[String, JsonSchema]): JsonObject[
    Map[String, JsonSchemaCodec]
  ] = JsonObject(properties.view.mapValues(fromJsonSchema).toMap)

  def simplyTyped(s: SchemaType): JsonSchemaCodec = JsonSchemaCodec.apply(
    `type` = Some(Left(s))
  )

  def fromSingular(
      overrideType: Option[List[SchemaType]]
  ): JsonSchema.Singular => JsonSchemaCodec = {
    case JsonSchema.Singular.String(format, minLength)
        if format.isEmpty && minLength.isEmpty && overrideType.exists(
          _.isEmpty
        ) =>
      Fix(Left(true))
    case JsonSchema.Singular.String(format, minLength) =>
      JsonSchemaCodec.apply(
        `type` = overrideType
          .map {
            case Nil              => None
            case List(schemaType) => Some(Left(schemaType))
            case schemaTypes      => Some(Right(JsonArray(schemaTypes)))
          }
          .getOrElse(Some(Left(SchemaType.String))),
        format = format,
        minLength = minLength
      )
    case JsonSchema.Singular.True    => Fix(Left(true))
    case JsonSchema.Singular.Null    => simplyTyped(SchemaType.Null)
    case JsonSchema.Singular.Integer => simplyTyped(SchemaType.Integer)
    case JsonSchema.Singular.Object(
          JsonSchema.ObjectValues(
            properties,
            required,
            additionalProperties
          )
        ) =>
      JsonSchemaCodec.apply(
        `type` = overrideType match {
          case Some(Nil)              => None
          case Some(List(schemaType)) => Some(Left(schemaType))
          case Some(schemaTypes)      => Some(Right(JsonArray(schemaTypes)))
          case None                   => Some(Left(SchemaType.Object))
        },
        properties = properties.map(encodeProperties),
        required = required.map(JsonArray(_)),
        additionalProperties = additionalProperties.map(fromJsonSchema)
      )
    case JsonSchema.Singular.Boolean =>
      simplyTyped(SchemaType.Boolean)
    case JsonSchema.Singular.Number =>
      simplyTyped(SchemaType.Number)
  }

  def fromJsonSchema: JsonSchema => JsonSchemaCodec = {
    case JsonSchema(schemas) =>
      schemas match {
        case List(schema) => fromSingular(None)(schema)
        case schemas =>
          val `type` = Some(
            singular(schemas.flatMap(SchemaType.fromSingular).distinct)
              .map(JsonArray(_))
          )
          schemas.flatMap(JsonSchema.ComplexType.fromSingular) match {
            case List(
                  JsonSchema.ComplexType.Object(
                    values
                  )
                ) =>
              fromSingular(
                Some(schemas.flatMap(SchemaType.fromSingular).distinct)
              )(
                JsonSchema.Singular.Object(
                  values
                )
              )
            case Nil =>
              JsonSchemaCodec.apply(
                `type` = `type`
              )
            case _ =>
              JsonSchemaCodec.apply(
                `type` = `type`,
                anyOf = Some {
                  JsonArray(
                    schemas.map(fromSingular(Some(List.empty)))
                  )
                }
              )
          }
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
//  given [A: SchemaOf]: SchemaOf[JsonArray[A]] with
//    def apply: JsonSchemaCodec = JsonSchemaCodec(
//      `type` = Some(Left(SchemaType.Array)),
//      items = Some(summon[SchemaOf[A]].apply)
//    )
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
