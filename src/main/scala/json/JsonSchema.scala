package json

import org.http4s.Uri
import cats.syntax.option

enum SchemaType:
  case String
  case Object
  case Int
  case Boolean
  case Null
  case Array

type JsonSchemaCodec = Fix[JsonSchemaUnfixed]

type JsonSchemaUnfixed[A] = JsonObject[
  (
      Option[("type", SchemaType)],
      Option[("properties", JsonObject[Map[String, A]])],
      Option[("required", JsonArray[String])],
      Option[("additionalProperties", A)]
  )
]

case class JsonSchema(`type`: Option[SchemaType], properties: Option[Map[String, JsonSchema]], required: Option[JsonArray[String]], additionalProperties: Option[JsonSchema])
object JsonSchema:
  def toCodec: JsonSchema => JsonSchemaCodec = {
    case JsonSchema(t, properties, required, additionalProperties) => Fix(JsonObject(t.map(s => ("type", s)),properties.map(p => ("properties", p.mapValues(toCodec(_)).toMap)), required.map(r => ("required",r)), additionalProperties.map(a => ("additionalProperties", toCodec(a)))))
  }

enum TypesafeSchema[A]:
  case String extends TypesafeSchema[java.lang.String]
  case Int extends TypesafeSchema[Int]
  case Boolean extends TypesafeSchema[Boolean]
  case Null extends TypesafeSchema[JsonNull]
  case Array[A](schema: TypesafeSchema[A]) extends TypesafeSchema[JsonArray[A]]
  case Object[A](properties: JsonProperties[A])
      extends TypesafeSchema[JsonObject[A]]
  case Fix[F[_]](unfix: TypesafeSchema[F[json.Fix[F]]])
      extends TypesafeSchema[json.Fix[F]]

object TypesafeSchema:
  def toJsonSchema[A]: TypesafeSchema[A] => JsonSchema = {
    case TypesafeSchema.String => JsonSchema(Some(SchemaType.String), None, None, None)
    case TypesafeSchema.Int => JsonSchema(Some(SchemaType.Int), None, None, None)
    case TypesafeSchema.Boolean => JsonSchema(Some(SchemaType.Boolean), None, None, None)
    case TypesafeSchema.Null => JsonSchema(None, None, None, None)
    case TypesafeSchema.Array(_) => JsonSchema(Some(SchemaType.Array), None, None, None)
    case TypesafeSchema.Object(_) => JsonSchema(Some(SchemaType.Object), None, None, None)
    case TypesafeSchema.Fix(_) => JsonSchema(Some(SchemaType.Object), None, None, None)
  }


enum JsonProperties[A]:
  case Empty extends JsonProperties[EmptyTuple]
  case Required[A, B, C <: Tuple](
      head: (A, TypesafeSchema[B]),
      tail: JsonProperties[C],
  ) extends JsonProperties[(A, B) *: C]
  case Optional[A, B, C <: Tuple](
      head: (A, TypesafeSchema[B]),
      tail: JsonProperties[C],
  ) extends JsonProperties[Option[(A, B)] *: C]
  case Map[A, B](schema: TypesafeSchema[B]) extends JsonProperties[scala.collection.Map[A, B]]

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
      summon[DefaultProperties[C]].apply,
    )
  given opt[A: ValueOf, B: DefaultSchema, C <: Tuple: DefaultProperties]
      : DefaultProperties[Option[(A, B)] *: C] with
    def apply: JsonProperties[Option[(A, B)] *: C] = JsonProperties.Optional(
      (summon[ValueOf[A]].value, summon[DefaultSchema[B]].apply),
      summon[DefaultProperties[C]].apply
    )
  given [A, B: DefaultSchema]: DefaultProperties[scala.collection.Map[A, B]] with
    def apply: JsonProperties[scala.collection.Map[A, B]] = JsonProperties.Map[A, B](summon[DefaultSchema[B]].apply)

//val x: TypesafeSchema[JsonArray[JsonObject[(("id", String), ("name", String))]]] =
//  TypesafeSchema.Array(
//    TypesafeSchema.Object(
//      JsonProperties.Required(
//        ("id", TypesafeSchema.String),
//        JsonProperties.Required(("name", TypesafeSchema.String), JsonProperties.Empty)
//      )
//    )
//  )

//val y: JsonProperties[(("id", String), ("name", String))] =
//    ToProperties.toProperties((("id": "id"), TypesafeSchema.String), (("name": "name"), TypesafeSchema.String))

//object TypesafeSchema:
//  def obj[A, B](a: A)(using t: ToProperties[A, B]): TypesafeSchema[JsonObject[B]] =
//    Object(t.apply(a))

//trait ToProperties[A, B]:
//  def apply(properties: A): JsonProperties[B]
//
//object ToProperties:
//  given ToProperties[EmptyTuple, EmptyTuple] = _ => JsonProperties.Empty
//  given [A, B, S <: Tuple, T <: Tuple](using
//      t: ToProperties[S, T]
//  ): ToProperties[(A, TypesafeSchema[B]) *: S, (A, B) *: T] = { case pair *: s =>
//    JsonProperties.Required(pair, t.apply(s))
//  }
//  def toProperties[A, B](a: A)(using t: ToProperties[A, B]): JsonProperties[B] =
//    t.apply(a)
//
//extension [A, B](t: (A, TypesafeSchema[B]))
//  def **:[C <: Tuple](c: JsonProperties[C]): JsonProperties[(A, B) *: C] =
//    JsonProperties.Required(t, c)
//
//case class TypesafeSchema[A](`$schema`: Option[Uri] = None, `$id`: Option[Uri] = None, title: Option[String] = None, description: Option[String] = None, schemaType: SchemaType[A])
