package schemas

import json.*
import io.rw.app.data.JsonCodec.*

object UserSchemas:
  val authenticateUser: JsonSchemaCodec =
    summon[SchemaOf[WrappedUser[AuthenticateUser]]].apply
  val regiserUser: JsonSchemaCodec =
    summon[SchemaOf[WrappedUser[RegisterUser]]].apply
  val updateUser: JsonSchemaCodec =
    summon[SchemaOf[WrappedUser[UpdateUser]]].apply
