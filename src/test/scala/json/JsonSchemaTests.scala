package json

import io.circe.syntax.*
import io.circe.{Decoder, Json}
import io.circe.parser.parse
import json.JsonSchemaCodec.given
import json.SchemaType
import utest.*
import io.circe.literal.*
import io.circe
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
  def testFixed[A: SchemaOf](expectedSchema: Json) = {
    val schema = JsonSchemaCodec.of[A].asJson
    assert(schema == expectedSchema)
  }
  def testSimple[A: SchemaOf](typeName: String) = testFixed[A](parse(s"""{
          "type": "$typeName"
        }""").toOption.get)
  val tests = Tests {
    test("json") { testFixed[circe.Json](json"true") }
    test("email") {
      testFixed[Email](json"""{
        "type": "string",
        "format": "email"
      }""")
    }
    test("string") { testSimple[String]("string") }
    test("null") { testSimple[JsonNull]("null") }
    test("integer") { testSimple[Int]("integer") }
    test("object") { testSimple[circe.JsonObject]("object") }
    test("boolean") { testSimple[Boolean]("boolean") }
    test("number") { testSimple[Double]("number") }
    test("object with properties") {
      testFixed[JsonObject[
        (("required-key", String), Option[("optional-key", Int)])
      ]](parse(s"""
        {
          "type": "object",
          "properties": {
            "required-key": {
              "type": "string"
            },
            "optional-key": {
              "type": "integer"
            }
          },
          "required": [
            "required-key"
          ]
        }
      """).toOption.get)
    }
    test("string or null") {
      val schema = JsonSchemaCodec
        .fromJsonSchema(summon[SchemaOf[Either[String, JsonNull]]].apply)
        .asJson
      val expectedSchema = (t: Json) => parse(s"""{
          "type": $t
        }""")
      assert(
        maybeType(schema).flatMap(_.as[StrictSet[String]]) == Right(
          StrictSet(Set("string", "null"))
        )
      )
      assert(Right(schema) == maybeType(schema).flatMap(expectedSchema))
    }

    test("object or null") {
      val schema = JsonSchemaCodec
        .of[
          Either[JsonObject.Solo[("key", String)], JsonNull]
        ]
        .asJson
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
    }
    test("object or object") {
      val schema = JsonSchemaCodec
        .of[
          Either[JsonObject.Solo[("first", String)], JsonObject.Solo[
            ("second", Int)
          ]]
        ]
        .asJson

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
    }
    test("object with nullable key") {
      val schema = JsonSchemaCodec
        .of[
          JsonObject.Solo[("key", Nullable[String])]
        ]
        .asJson
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
    }
    test("object or object or object") {
      val schema =
        JsonSchemaCodec
          .of[Either[JsonObject.Solo[("first", String)], Either[
            JsonObject.Solo[("second", Int)],
            JsonObject.Solo[("third", Boolean)]
          ]]]
          .asJson

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
    }
    test("map object") {
      testFixed[JsonObject[Map[String, String]]](json"""{
        "type": "object",
        "additionalProperties": {
          "type": "string"
        }
      }""")
    }
    test("object or map object") {
      val schema = JsonSchemaCodec
        .of[
          Either[JsonObject.Solo[("key", String)], JsonObject[Map[String, Int]]]
        ]
        .asJson
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
      val schemaJson = JsonSchemaCodec.of[Either[String, Email]].asJson
      val expectedSchema = (anyOf: Json) => parse(s"""{
        "type": "string",
        "anyOf": $anyOf
      }""")
      val expectedFirstSchema = json"true"
      val expectedSecondSchema = json"""{
        "format": "email"
      }"""
      assert(
        maybeAnyOf(schemaJson).flatMap(_.as[StrictSet[Json]]) == Right(
          StrictSet(Set(expectedFirstSchema, expectedSecondSchema))
        )
      )
    }
    test("email or minLength") {
      type MyStringFormat
      given SchemaOf[MyStringFormat] with
        def apply: JsonSchema =
          JsonSchema.string(
            minLength = Some(5)
          )
      val schemaJson = JsonSchemaCodec.of[Either[MyStringFormat, Email]].asJson
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
        maybeAnyOf(schemaJson).flatMap(_.as[StrictSet[Json]]) == Right(
          StrictSet(Set(expectedFirstSchema, expectedSecondSchema))
        )
      )
      assert(
        Right(schemaJson) == maybeAnyOf(schemaJson).flatMap(expectedSchema)
      )
    }
  }

}
