package json

import io.circe.syntax.*
import io.circe.{Decoder, Json}
import io.circe.parser.parse
import json.JsonSchema.given
import utest.*
import io.circe.literal.*
import cats.syntax.all.*
import io.circe.DecodingFailure
import io.circe.DecodingFailure.Reason

object JsonSchemaTests extends TestSuite {
  def addToSet[A](s: Set[A], a: A) = if (s contains a) None else Some(s + a)

  def toSet[A](l: List[A]): Option[Set[A]] = l.foldM(Set.empty)(addToSet)

  case class StrictSet[A](s: Set[A])
  object StrictSet:
    given [A: Decoder]: Decoder[StrictSet[A]] =
      Decoder[List[A]].emap(
        toSet(_).toRight(s"contains duplicates").map(StrictSet(_))
      )
  val maybeType = Decoder[Json].at("type").decodeJson

  val tests = Tests {
    test("string or null") {
      val schema = summon[SchemaOf[Either[String, JsonNull]]].apply.asJson
      val expectedSchema = (t: Json) => parse(s"""{
          "type": $t
        }""")
      assert(
        maybeType(schema).flatMap(_.as[StrictSet[String]]) == Right(
          StrictSet(Set("string", "null"))
        )
      )
      assert(Right(schema) == maybeType(schema).flatMap(expectedSchema))
      schema
    }
    test("object or null") {
      val schema = summon[SchemaOf[
        Either[JsonObject.Solo[("key", String)], JsonNull]
      ]].apply.asJson
      val expectedSchema = (t: Json) => parse(s"""{
          "type": $t,
          "properties": {
            "key": {
              "type": "string"
            }
          },
          "required": ["key"]
        }""")
      assert(
        maybeType(schema).flatMap(_.as[StrictSet[String]]) == Right(
          StrictSet(Set("null", "object"))
        )
      )
      assert(Right(schema) == maybeType(schema).flatMap(expectedSchema))
      schema
    }
    test("object or object") {
      val schema = summon[SchemaOf[
        Either[JsonObject.Solo[("first", String)], JsonObject.Solo[
          ("second", Int)
        ]]
      ]].apply.asJson

      val expectedSchema = (anyOf: Json) => parse(s"""{
          "type": "object",
          "anyOf": $anyOf
        }""")
      val maybeAnyOf = Decoder[Json].at("anyOf").decodeJson
      val expectedFirstSchema = json"""{
        "properties": {
          "first": {
            "type": "string"
          },
          "required": ["first"]
        }
      }"""
      val expectedSecondSchema = json"""{
        "properties": {
          "second": {
            "type": "string"
          },
          "required": ["second"]
        }
      }"""
      assert(
        maybeType(schema).flatMap(_.as[StrictSet[Json]]) == Right(
          StrictSet(Set(expectedFirstSchema, expectedSecondSchema))
        )
      )
      // assert(Right(schema) == maybeType(schema).flatMap(expectedSchema))
      schema
    }
  }
}
