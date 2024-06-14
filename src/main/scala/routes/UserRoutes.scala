package io.rw.app.routes

import cats.effect.{Async, Concurrent}
import cats.Functor
import cats.data.EitherT
import cats.implicits.*
import io.rw.app.apis.*
import io.rw.app.data.{AuthUser, ApiErrors}
import io.rw.app.data.AuthUser
import io.rw.app.data.ApiInputs.*
import io.rw.app.data.RequestBodies.*
import io.rw.app.data.{JsonCodec, User}
import io.rw.app.valiation.*
import io.circe.{Json, Decoder}
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.dsl.Http4sDsl
import json.{JsonObject, given}
import io.rw.app.data.ApiError
import cats.MonadThrow
import io.rw.app.data.JsonCodec.WrappedUser

object UserRoutes {
  def toOutput[F[_]: Functor]
      : F[User] => F[JsonCodec.WrappedUser[JsonCodec.User]] =
    _.map(JsonCodec.WrappedUser.apply compose JsonCodec.User.fromData)

  def decodeJsonRequest[F[_]: Concurrent, A: Decoder](
      request: Request[F]
  ): EitherT[F, ApiError, A] = EitherT(
    request.as[Json].map(_.as[A].leftMap(e => ApiErrors.InvalidJson(e.message)))
  )

  def apply[F[_]: Async](users: UserApis[F]): AppRoutes[F] = {

    implicit val dsl = Http4sDsl.apply[F]
    import dsl.*

    AuthedRoutes.of[Option[AuthUser], F] {
      case rq @ POST -> Root / "users" / "login" as _ =>
        for {
          body <- rq.req.as[JsonCodec.WrappedUser[JsonCodec.AuthenticateUser]]
          rs <-
            users
              .authenticate(
                AuthenticateUserInput.fromCodec(JsonObject.getSoloValue(body))
              )
              .map(toOutput)
              .flatMap(toResponse)
        } yield rs

      case rq @ POST -> Root / "users" as _ =>
        {
          for {
            body <- decodeJsonRequest[F, JsonCodec.WrappedUser[
              JsonCodec.RegisterUser
            ]](rq.req)
            user <-
              users
                .register(
                  RegisterUserInput.fromCodec(JsonObject.getSoloValue(body))
                )
          } yield JsonCodec.WrappedUser.apply(JsonCodec.User.fromData(user))
        }.value.flatMap(toResponse)

      case GET -> Root / "user" as authUser =>
        withAuthUser(authUser) { u =>
          users
            .get(GetUserInput(u))
            .map(toOutput)
            .flatMap(toResponse)
        }

      case rq @ PUT -> Root / "user" as authUser => {
        for {
          body <- rq.req.as[JsonCodec.WrappedUser[JsonCodec.UpdateUser]]
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
                .map(toOutput)
                .flatMap(toResponse)
            }
          }
        } yield rs
      }
    }
  }
}
