package io.rw.app.routes

import cats.effect.Sync
import cats.implicits._
import io.circe.generic.auto._
import io.rw.app.apis._
import io.rw.app.data.AuthUser
import io.rw.app.data.ApiInputs._
import io.rw.app.data.RequestBodies._
import io.rw.app.valiation._
import org.http4s._
import org.http4s.circe.CirceEntityCodec._
import org.http4s.dsl.Http4sDsl

object UserRoutes {

  def apply[F[_] : Sync](users: UserApis[F]): AppRoutes[F] = {

    implicit val dsl = Http4sDsl.apply[F]
    import dsl._

    AuthedRoutes.of[Option[AuthUser], F] {
      case rq @ POST -> Root / "users" / "login" as _ =>
        for {
          body <- rq.req.as[WrappedUserBody[AuthenticateUserBody]]
          rs <- withValidation(validAuthenticateUserBody(body.user)) { valid =>
            users.authenticate(AuthenticateUserInput(valid.email, valid.password)).flatMap(toResponse(_))
          }
        } yield rs

      case rq @ POST -> Root / "users" as _ =>
        for {
          body <- rq.req.as[WrappedUserBody[RegisterUserBody]]
          rs <- withValidation(validRegisterUserBody(body.user)) { valid =>
            users.register(RegisterUserInput(valid.username, valid.email, valid.password)).flatMap(toResponse(_))
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
              users.update(UpdateUserInput(u, valid.username, valid.email, valid.password, valid.bio, valid.image)).flatMap(toResponse(_))
            }
          }
        } yield rs
      }
    }
  }
}
