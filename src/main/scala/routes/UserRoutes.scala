package io.rw.app.routes

import cats.effect.Async
import cats.implicits.*
import io.rw.app.apis.*
import io.rw.app.data.AuthUser
import io.rw.app.data.ApiInputs.*
import io.rw.app.data.RequestBodies.*
import io.rw.app.valiation.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.dsl.Http4sDsl
import json.*

object UserRoutes {

  def apply[F[_]: Async](users: UserApis[F]): AppRoutes[F] = {

    implicit val dsl = Http4sDsl.apply[F]
    import dsl.*

    AuthedRoutes.of[Option[AuthUser], F] {
      case rq @ POST -> Root / "users" / "login" as _ =>
        for {
          WrappedUserBody(JsonObject(((_, email), (_, password)))) <- rq.req
            .as[WrappedUserBody[AuthenticateUserBody]]
          rs <- withValidation(validEmail(email)) { email =>
            users
              .authenticate(AuthenticateUserInput(email, password))
              .flatMap(toResponse(_))
          }
        } yield rs

      case rq @ POST -> Root / "users" as _ =>
        for {
          WrappedUserBody(JsonObject(((_, username), (_, email), (_, passord)))) <- rq.req.as[WrappedUserBody[RegisterUserBody]]
          rs <- withValidation((validUsername(username), validEmail(email), validPassword(password)).tupled) { case (username, email, password) =>
            users
              .register(
                RegisterUserInput(username, email, password)
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
