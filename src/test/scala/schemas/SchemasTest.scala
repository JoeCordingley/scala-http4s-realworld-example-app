package schemas

import utest.*
import io.circe.syntax.*
import io.circe.Encoder
import json.*
import json.given
import json.JsonSchema.given
import io.circe.literal.*

object SchemasTest extends TestSuite {

  val tests = Tests {
    test("assert true") {
      assert(true)
    }
    test("authenticate user schema") {
      val x = UserSchemas.authenticateUser.asJson
      assert(x == json"""{
        "type": "object",
        "properties": {
          "user": {
            "type": "object",
            "properties": {
              "email": {
                "type": "string",
                "format": "email"
              },
              "password": {
                "type": "string",
                "minLength": 8,
                "maxLength": 100
              }
            },
            "required": ["email", "password"]
          }
        },
        "required": ["user"]
      }""")
      x
    }
  }
}
