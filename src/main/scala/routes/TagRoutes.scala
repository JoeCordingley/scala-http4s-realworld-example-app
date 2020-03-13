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

object TagRoutes {

  def apply[F[_] : Sync](tags: TagApis[F]): AppRoutes[F] = {

    implicit val dsl = Http4sDsl.apply[F]
    import dsl._

    AuthedRoutes.of[Option[AuthUser], F] {
      case GET -> Root / "tags" as _ =>
        tags.get(GetTagsInput()).flatMap(toResponse(_))
    }
  }
}
