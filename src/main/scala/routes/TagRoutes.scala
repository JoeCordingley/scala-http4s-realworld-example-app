package io.rw.app.routes

import cats.effect.Sync
import cats.implicits.*
import io.rw.app.apis.*
import io.rw.app.data.{AuthUser, JsonCodec}
import io.rw.app.data.ApiInputs.*
import io.rw.app.data.RequestBodies.*
import io.rw.app.validation.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.dsl.Http4sDsl
import json.given

object TagRoutes {

  def apply[F[_]: Sync](tags: TagApis[F]): AppRoutes[F] = {

    implicit val dsl = Http4sDsl.apply[F]
    import dsl.*

    AuthedRoutes.of[Option[AuthUser], F] { case GET -> Root / "tags" as _ =>
      tags
        .get(GetTagsInput())
        .map(_.map(JsonCodec.Tags.fromData))
        .flatMap(toResponse)
    }
  }
}
