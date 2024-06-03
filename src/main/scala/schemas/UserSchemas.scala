package schemas

import json.*
import io.rw.app.data.JsonCodec.{
  GetArticlesOutput,
  AuthenticateUser,
  WrappedUser
}

object UserSchemas:
  val authenticateUser: JsonSchema =
    summon[SchemaOf[WrappedUser[AuthenticateUser]]].apply
