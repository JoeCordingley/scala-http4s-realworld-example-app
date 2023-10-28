package io.rw.app.routes

import cats.effect.Async
import cats.implicits.*
import io.rw.app.apis.*
import io.rw.app.data.AuthUser
import io.rw.app.data.ApiInputs.*
import io.rw.app.data.RequestBodies.*
import io.rw.app.data.JsonCodec.*
import io.rw.app.valiation.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.dsl.Http4sDsl
import json.{JsonObject, given}

object UserRoutes {

  def apply[F[_]: Async](users: UserApis[F]): AppRoutes[F] = {

    implicit val dsl = Http4sDsl.apply[F]
    import dsl.*

    AuthedRoutes.of[Option[AuthUser], F] {
      case rq @ POST -> Root / "users" / "login" as _ =>
        for {
          body <- rq.req.as[User[AuthenticateUser]]
          rs <- withValidation(
            validAuthenticateUserBody(
              AuthenticateUserBody.fromCodec(JsonObject.getSoloValue(body))
            )
          ) { valid =>
            users
              .authenticate(AuthenticateUserInput(valid.email, valid.password))
              .flatMap(toResponse(_))
          }
        } yield rs

      case rq @ POST -> Root / "users" as _ =>
        for {
          body <- rq.req.as[User[RegisterUser]]
          rs <- withValidation(
            validRegisterUserBody(
              RegisterUserBody.fromCodec(JsonObject.getSoloValue(body))
            )
          ) { valid =>
            users
              .register(
                RegisterUserInput(valid.username, valid.email, valid.password)
              )
              .flatMap(toResponse(_))
          }
        } yield rs

      case GET -> Root / "user" as authUser =>
        withAuthUser(authUser) { u =>
          users.get(GetUserInput(u)).flatMap(toResponse(_))
        }

      case rq @ PUT -> Root / "user" as authUser => {
        for {
          body <- rq.req.as[User[UpdateUser]]
          rs <- withAuthUser(authUser) { u =>
            withValidation(
              validUpdateUserBody(
                UpdateUserBody.fromCodec(JsonObject.getSoloValue(body))
              )
            ) { valid =>
              users
                .update(
                  UpdateUserInput(
                    u,
                    valid.username,
                    valid.email,
                    valid.password,
                    valid.bio,
                    valid.image
                  )
                )
                .flatMap(toResponse(_))
            }
          }
        } yield rs
      }
    }
  }
}
