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

type JsonSchemaCodec = json.Fix[JsonSchemaUnfixed]
object JsonSchemaCodec:
  def apply(
      `type`: Option[SchemaType] = None,
      properties: Option[Map[String, JsonSchemaCodec]] = None,
      required: Option[List[String]] = None,
      items: Option[JsonSchemaCodec] = None,
      additionalProperties: Option[JsonSchemaCodec] = None
  ): JsonSchemaCodec = Fix(
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

type JsonSchemaUnfixed[A] = JsonObject[
  (
      Option[("type", SchemaType)],
      Option[("properties", JsonObject[Map[String, A]])],
      Option[("required", JsonArray[String])],
      Option[("items", A)],
      Option[("additionalProperties", A)]
  )
]

enum TypesafeSchema[A]:
  case String extends TypesafeSchema[java.lang.String]
  case Int extends TypesafeSchema[Int]
  case Double extends TypesafeSchema[Double]
  case Boolean extends TypesafeSchema[Boolean]
  case Null extends TypesafeSchema[JsonNull]
  case Array[A](schema: TypesafeSchema[A]) extends TypesafeSchema[JsonArray[A]]
  case Object[A](properties: JsonProperties[A])
      extends TypesafeSchema[JsonObject[A]]
  case Fix[F[_]](unfix: TypesafeSchema[F[json.Fix[F]]])
      extends TypesafeSchema[json.Fix[F]]

trait ToJsonCodec[A]:
  def apply(a: A): JsonSchemaCodec

object ToJsonCodec:
  given string: ToJsonCodec[TypesafeSchema[String]] =
    case TypesafeSchema.String =>
      JsonSchemaCodec(`type` = Some(SchemaType.String))
  given int: ToJsonCodec[TypesafeSchema[Int]] =
    case TypesafeSchema.Int =>
      JsonSchemaCodec(`type` = Some(SchemaType.Integer))
  given bool: ToJsonCodec[TypesafeSchema[Boolean]] =
    case TypesafeSchema.Boolean =>
      JsonSchemaCodec(`type` = Some(SchemaType.Boolean))
  given `null`: ToJsonCodec[TypesafeSchema[JsonNull]] =
    case TypesafeSchema.Null =>
      JsonSchemaCodec(`type` = Some(SchemaType.Null))
  given double: ToJsonCodec[TypesafeSchema[Double]] =
    case TypesafeSchema.Double =>
      JsonSchemaCodec(`type` = Some(SchemaType.Number))
  given arr[A](using
      c: ToJsonCodec[TypesafeSchema[A]]
  ): ToJsonCodec[TypesafeSchema[JsonArray[A]]] =
    case TypesafeSchema.Array(schema) =>
      JsonSchemaCodec(
        `type` = Some(SchemaType.Array),
        items = Some(c.apply(schema))
      )

object TypesafeSchema:

  def toCodec[A]: TypesafeSchema[A] => JsonSchemaCodec = {
    case TypesafeSchema.String =>
      JsonSchemaCodec(`type` = Some(SchemaType.String))
    case TypesafeSchema.Int =>
      JsonSchemaCodec(`type` = Some(SchemaType.Integer))
    case TypesafeSchema.Boolean =>
      JsonSchemaCodec(`type` = Some(SchemaType.Boolean))
    case TypesafeSchema.Null =>
      JsonSchemaCodec(`type` = Some(SchemaType.Null))
    case TypesafeSchema.Double =>
      JsonSchemaCodec(`type` = Some(SchemaType.Number))
    case TypesafeSchema.Array(schema) =>
      JsonSchemaCodec(
        `type` = Some(SchemaType.Array),
        items = Some(toCodec(schema))
      )
    case TypesafeSchema.Object(properties) =>
      JsonSchemaCodec(`type` = Some(SchemaType.Object))
    case TypesafeSchema.Fix(_) => JsonSchemaCodec()
  }

enum JsonProperties[A]:
  case Empty extends JsonProperties[EmptyTuple]
  case Required[A, B, C <: Tuple](
      head: (A, TypesafeSchema[B]),
      tail: JsonProperties[C]
  ) extends JsonProperties[(A, B) *: C]
  case Optional[A, B, C <: Tuple](
      head: (A, TypesafeSchema[B]),
      tail: JsonProperties[C]
  ) extends JsonProperties[Option[(A, B)] *: C]
  case Map[A, B](schema: TypesafeSchema[B])
      extends JsonProperties[scala.collection.Map[A, B]]

//object JsonProperties:
//  def toMap[A: ToPropertyMap](
//      properties: JsonProperties[A]
//  ): Map[String, JsonSchemaCodec]

trait ToPropertyMap[A]

trait DefaultSchema[A]:
  def apply: TypesafeSchema[A]

object DefaultSchema:
  given DefaultSchema[String] with
    def apply: TypesafeSchema[String] = TypesafeSchema.String
  given DefaultSchema[Int] with
    def apply: TypesafeSchema[Int] = TypesafeSchema.Int
  given DefaultSchema[Boolean] with
    def apply: TypesafeSchema[Boolean] = TypesafeSchema.Boolean
  given [A: DefaultSchema]: DefaultSchema[JsonArray[A]] with
    def apply: TypesafeSchema[JsonArray[A]] =
      TypesafeSchema.Array(summon[DefaultSchema[A]].apply)
  given [A: DefaultProperties]: DefaultSchema[JsonObject[A]] with
    def apply: TypesafeSchema[JsonObject[A]] =
      TypesafeSchema.Object(summon[DefaultProperties[A]].apply)
  given [F[_]](using d: => DefaultSchema[F[Fix[F]]]): DefaultSchema[Fix[F]] with
    def apply: TypesafeSchema[Fix[F]] = TypesafeSchema.Fix(d.apply)

trait DefaultProperties[A]:
  def apply: JsonProperties[A]

object DefaultProperties:
  given DefaultProperties[EmptyTuple] with
    def apply: JsonProperties[EmptyTuple] = JsonProperties.Empty
  given nonOpt[A: ValueOf, B: DefaultSchema, C <: Tuple: DefaultProperties]
      : DefaultProperties[(A, B) *: C] with
    def apply: JsonProperties[(A, B) *: C] = JsonProperties.Required(
      (summon[ValueOf[A]].value, summon[DefaultSchema[B]].apply),
      summon[DefaultProperties[C]].apply
    )
  given opt[A: ValueOf, B: DefaultSchema, C <: Tuple: DefaultProperties]
      : DefaultProperties[Option[(A, B)] *: C] with
    def apply: JsonProperties[Option[(A, B)] *: C] = JsonProperties.Optional(
      (summon[ValueOf[A]].value, summon[DefaultSchema[B]].apply),
      summon[DefaultProperties[C]].apply
    )
  given [A, B: DefaultSchema]: DefaultProperties[scala.collection.Map[A, B]]
  with
    def apply: JsonProperties[scala.collection.Map[A, B]] =
      JsonProperties.Map[A, B](summon[DefaultSchema[B]].apply)
