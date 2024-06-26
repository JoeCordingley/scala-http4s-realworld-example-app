package io.rw.app.routes

import cats.effect.Async
import cats.implicits.*
import io.rw.app.apis.*
import io.rw.app.data.AuthUser
import io.rw.app.data.ApiInputs.*
import io.rw.app.data.RequestBodies.*
import io.rw.app.data.JsonCodec
import io.rw.app.validation.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.dsl.Http4sDsl
import json.{JsonObject, given}

object CommentRoutes {

  def apply[F[_]: Async](comments: CommentApis[F]): AppRoutes[F] = {

    implicit val dsl = Http4sDsl.apply[F]
    import dsl.*

    AuthedRoutes.of[Option[AuthUser], F] {
      case rq @ POST -> Root / "articles" / slug / "comments" as authUser =>
        for {
          body <- rq.req.as[JsonCodec.WrappedComment[JsonCodec.Body]]
          rs <- withAuthUser(authUser) { u =>
            // TODO extension methods
            withValidation(
              validAddCommentBody(
                AddCommentBody(
                  JsonObject.getSoloValue(JsonObject.getSoloValue(body))
                )
              )
            ) { valid =>
              comments
                .add(AddCommentInput(u, slug, valid.body))
                .map(
                  _.map(
                    JsonCodec.WrappedComment.apply compose JsonCodec.Comment.fromData
                  )
                )
                .flatMap(toResponse)
            }
          }
        } yield rs

      case GET -> Root / "articles" / slug / "comments" as authUser =>
        comments
          .get(GetCommentsInput(authUser, slug))
          .map(_.map(JsonCodec.Comments.fromData))
          .flatMap(toResponse)

      case DELETE -> Root / "articles" / slug / "comments" / IntVar(
            commentId
          ) as authUser =>
        withAuthUser(authUser) { u =>
          comments
            .delete(DeleteCommentInput(u, slug, commentId))
            .map(_.as(JsonObject.empty))
            .flatMap(toResponse)
        }
    }
  }
}
