package io.rw.app.routes

import cats.effect.Async
import cats.implicits.*
import io.rw.app.apis.*
import io.rw.app.data.{AuthUser, Email, Password, Username}
import io.rw.app.data.ApiInputs.*
import io.rw.app.data.RequestBodies.*
import io.rw.app.data.JsonCodec.*
import io.rw.app.valiation.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.dsl.Http4sDsl
import json.JsonObject

object UserRoutes {

  def apply[F[_]: Async](users: UserApis[F]): AppRoutes[F] = {

    implicit val dsl = Http4sDsl.apply[F]
    import dsl.*

    AuthedRoutes.of[Option[AuthUser], F] {
      case rq @ POST -> Root / "users" / "login" as _ =>
        for {
          JsonObject((_, JsonObject((_, Email(email)), (_, Password(password)))) *: EmptyTuple) <- rq.req.as[User[AuthenticateUser]]
          rs <- withValidation(validAuthenticateUserBody(AuthenticateUserBody(email = email, password = password))) { valid =>
            users
              .authenticate(AuthenticateUserInput(valid.email, valid.password))
              .flatMap(toResponse(_))
          }
        } yield rs

      case rq @ POST -> Root / "users" as _ =>
        for {
          JsonObject((_, JsonObject((_, Username(username)), (_, Email(email)), (_, Password(password)))) *: EmptyTuple) <- rq.req.as[User[RegisterUser]]
          rs <- withValidation(validRegisterUserBody(RegisterUserBody(username = username, email = email, password = password))) { valid =>
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
          body <- rq.req.as[WrappedUserBody[UpdateUserBody]]
          rs <- withAuthUser(authUser) { u =>
            withValidation(validUpdateUserBody(body.user)) { valid =>
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
