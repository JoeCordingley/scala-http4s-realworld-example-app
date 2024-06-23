package schemas

import json.*
import io.rw.app.data.JsonCodec.*

object UserSchemas:
  val authenticateUser: JsonSchema =
    summon[SchemaOf[WrappedUser[AuthenticateUser]]].apply
  val regiserUser: JsonSchema =
    summon[SchemaOf[WrappedUser[RegisterUser]]].apply
  val updateUser: JsonSchema =
    summon[SchemaOf[WrappedUser[UpdateUser]]].apply
