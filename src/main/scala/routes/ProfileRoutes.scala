package io.rw.app.routes

import cats.effect.Sync
import cats.implicits.*
import io.rw.app.apis.*
import io.rw.app.data.{AuthUser, JsonCodec}
import io.rw.app.data.ApiInputs.*
import io.rw.app.data.RequestBodies.*
import io.rw.app.valiation.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.dsl.Http4sDsl
import json.given

object ProfileRoutes {

  def apply[F[_]: Sync](profiles: ProfileApis[F]): AppRoutes[F] = {

    implicit val dsl = Http4sDsl.apply[F]
    import dsl.*

    AuthedRoutes.of[Option[AuthUser], F] {
      case GET -> Root / "profiles" / username as authUser =>
        profiles
          .get(GetProfileInput(authUser, username))
          .map(_.map(JsonCodec.WrappedProfile.fromData))
          .flatMap(toResponse)

      case POST -> Root / "profiles" / username / "follow" as authUser =>
        withAuthUser(authUser) { u =>
          profiles
            .follow(FollowUserInput(u, username))
            .map(_.map(JsonCodec.WrappedProfile.fromData))
            .flatMap(toResponse)
        }

      case DELETE -> Root / "profiles" / username / "follow" as authUser =>
        withAuthUser(authUser) { u =>
          profiles
            .unfollow(UnfollowUserInput(u, username))
            .map(_.map(JsonCodec.WrappedProfile.fromData))
            .flatMap(toResponse)
        }
    }
  }
}
