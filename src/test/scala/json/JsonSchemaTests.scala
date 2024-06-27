package json

import io.circe.syntax.*
import io.circe.{Decoder, Json}
import io.circe.parser.parse
import json.JsonSchema.given
import json.SchemaType
import utest.*
import io.circe.literal.*
import cats.syntax.all.*
import io.circe.DecodingFailure
import io.circe.DecodingFailure.Reason
import scala.annotation.experimental

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
  val maybeAnyOf = Decoder[Json].at("anyOf").decodeJson

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
          }
        },
        "required": ["first"]
      }"""
      val expectedSecondSchema = json"""{
        "properties": {
          "second": {
            "type": "integer"
          }
        },
        "required": ["second"]
      }"""
      assert(
        maybeAnyOf(schema).flatMap(_.as[StrictSet[Json]]) == Right(
          StrictSet(Set(expectedFirstSchema, expectedSecondSchema))
        )
      )
      assert(Right(schema) == maybeAnyOf(schema).flatMap(expectedSchema))
      schema
    }
    test("object with nullable key") {
      val schema = summon[SchemaOf[
        JsonObject.Solo[("key", Nullable[String])]
      ]].apply.asJson
      val maybeKeyType =
        Decoder[Json].at("type").at("key").at("properties").decodeJson
      val expectedSchema = (j: Json) => parse(s"""{
          "type": "object",
          "properties": {
            "key": {
              "type": $j
            }
          },
          "required": ["key"]
        }""")
      assert(
        maybeKeyType(schema).flatMap(_.as[StrictSet[String]]) == Right(
          StrictSet(Set("string", "null"))
        )
      )
      assert(Right(schema) == maybeKeyType(schema).flatMap(expectedSchema))
      schema
    }
    test("object or object or object") {
      val schema =
        summon[SchemaOf[Either[JsonObject.Solo[("first", String)], Either[
          JsonObject.Solo[("second", Int)],
          JsonObject.Solo[("third", Boolean)]
        ]]]].apply.asJson

      val expectedSchema = (anyOf: Json) => parse(s"""{
          "type": "object",
          "anyOf": $anyOf
        }""")
      val expectedFirstSchema = json"""{
        "properties": {
          "first": {
            "type": "string"
          }
        },
        "required": ["first"]
      }"""
      val expectedSecondSchema = json"""{
        "properties": {
          "second": {
            "type": "integer"
          }
        },
        "required": ["second"]
      }"""
      val expectedThirdSchema = json"""{
        "properties": {
          "third": {
            "type": "boolean"
          }
        },
        "required": ["third"]
      }"""
      assert(
        maybeAnyOf(schema).flatMap(_.as[StrictSet[Json]]) == Right(
          StrictSet(
            Set(expectedFirstSchema, expectedSecondSchema, expectedThirdSchema)
          )
        )
      )
      assert(Right(schema) == maybeAnyOf(schema).flatMap(expectedSchema))
      schema
    }
    test("map object") {
      val schema =
        summon[SchemaOf[JsonObject[Map[String, String]]]].apply.asJson
      val expectedSchema = json"""{
        "type": "object",
        "additionalProperties": {
          "type": "string"
        }
      }"""
      assert(schema == expectedSchema)
    }
    test("object or map object") {
      val schema = summon[SchemaOf[
        Either[JsonObject.Solo[("key", String)], JsonObject[Map[String, Int]]]
      ]].apply.asJson
      val expectedSchema = (anyOf: Json) => parse(s"""{
        "type": "object",
        "anyOf": $anyOf
      }""")
      val expectedFirstSchema = json"""{
        "properties": {
          "key": {
            "type": "string"
          }
        },
        "required": ["key"]
      }"""
      val expectedSecondSchema = json"""{
        "additionalProperties": {
          "type": "integer"
        }
      }"""
      assert(
        maybeAnyOf(schema).flatMap(_.as[StrictSet[Json]]) == Right(
          StrictSet(Set(expectedFirstSchema, expectedSecondSchema))
        )
      )
      assert(Right(schema) == maybeAnyOf(schema).flatMap(expectedSchema))
    }
    test("string or formatted string") {
      type MyStringFormat
      given SchemaOf[MyStringFormat] with
        def apply: JsonSchema =
          JsonSchema(
            `type` = Some(Left(SchemaType.String)),
            minLength = Some(5)
          )
      val schema = summon[SchemaOf[Either[MyStringFormat, Email]]].apply.asJson
      val expectedSchema = (anyOf: Json) => parse(s"""{
        "type": "string",
        "anyOf": $anyOf
      }""")
      val expectedFirstSchema = json"""{
        "format": "email"
      }"""
      val expectedSecondSchema = json"""{
        "minLength": 5
      }"""
      assert(
        maybeAnyOf(schema).flatMap(_.as[StrictSet[Json]]) == Right(
          StrictSet(Set(expectedFirstSchema, expectedSecondSchema))
        )
      )
      assert(Right(schema) == maybeAnyOf(schema).flatMap(expectedSchema))
    }
  }

}
