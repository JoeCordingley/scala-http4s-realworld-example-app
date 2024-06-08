package schemas

import json.*
import io.rw.app.data.JsonCodec.{
  GetArticlesOutput,
  AuthenticateUser,
  WrappedUser
}
import io.rw.app.data.JsonCodec.RegisterUser

object UserSchemas:
  val authenticateUser: JsonSchema =
    summon[SchemaOf[WrappedUser[AuthenticateUser]]].apply
  val regiserUser: JsonSchema =
    summon[SchemaOf[WrappedUser[RegisterUser]]].apply
