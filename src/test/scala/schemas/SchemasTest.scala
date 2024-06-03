package schemas

import utest.*
import io.circe.syntax.*
import io.circe.Encoder
import json.*

object SchemasTest extends TestSuite {
  // type ProgressiveJsonCodec = json.Fix[[A] =>> JsonObject[
  //  (
  //      Option[("type", SchemaType)],
  //      Option[("properties", JsonObject[Map[String, A]])],
  //  )
  // ]]
  // type ProgressiveJsonCodec = json.Fix[[A] =>> JsonObject[
  //  ("properties", A) *: EmptyTuple
  // ]]
  type ProgressiveJsonCodec = json.Fix[UnfixedP]

  def unfixEncoder[A: Encoder]: Encoder[UnfixedP[A]] = summon

  type UnfixedP[A] = JsonObject[
    ("properties", JsonObject[Map[String, A]]) *: EmptyTuple
  ]
  // val e = summon[Encoder[ProgressiveJsonCodec]]

  val tests = Tests {
    test("assert true") {
      assert(true)
    }
//    test("assert true") {
//      assert(UserSchemas.authenticateUser.asJson == json"""{}""")
//    }
  }
}
