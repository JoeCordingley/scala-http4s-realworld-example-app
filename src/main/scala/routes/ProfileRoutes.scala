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

object ProfileRoutes {

  def apply[F[_] : Sync](profiles: ProfileApis[F]): AppRoutes[F] = {

    implicit val dsl = Http4sDsl.apply[F]
    import dsl._

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
