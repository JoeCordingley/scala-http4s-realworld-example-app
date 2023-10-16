package io.rw.app.routes

import cats.effect.Sync
import cats.implicits.*
import io.circe.generic.auto.*
import io.rw.app.apis.*
import io.rw.app.data.AuthUser
import io.rw.app.data.ApiInputs.*
import io.rw.app.data.RequestBodies.*
import io.rw.app.valiation.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.dsl.Http4sDsl

object ProfileRoutes {

  def apply[F[_] : Sync](profiles: ProfileApis[F]): AppRoutes[F] = {

    implicit val dsl = Http4sDsl.apply[F]
    import dsl.*

    AuthedRoutes.of[Option[AuthUser], F] {
      case GET -> Root / "profiles" / username as authUser =>
        profiles.get(GetProfileInput(authUser, username)).flatMap(toResponse(_))

      case POST -> Root / "profiles" / username / "follow" as authUser =>
        withAuthUser(authUser) { u =>
          profiles.follow(FollowUserInput(u, username)).flatMap(toResponse(_))
        }

      case DELETE -> Root / "profiles" / username / "follow" as authUser =>
        withAuthUser(authUser) { u =>
          profiles.unfollow(UnfollowUserInput(u, username)).flatMap(toResponse(_))
        }
    }
  }
}
