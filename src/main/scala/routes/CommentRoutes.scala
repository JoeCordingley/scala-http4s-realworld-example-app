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

object CommentRoutes {

  def apply[F[_] : Sync](comments: CommentApis[F]): AppRoutes[F] = {

    implicit val dsl = Http4sDsl.apply[F]
    import dsl._

    AuthedRoutes.of[Option[AuthUser], F] {
      case rq @ POST -> Root / "articles" / slug / "comments" as authUser =>
        for {
          body <- rq.req.as[WrappedCommentBody[AddCommentBody]]
          rs <- withAuthUser(authUser) { u =>
            withValidation(validAddCommentBody(body.comment)) { valid =>
              comments.add(AddCommentInput(u, slug, valid.body)).flatMap(toResponse(_))
            }
          }
        } yield rs

      case GET -> Root / "articles" / slug / "comments" as authUser =>
        comments.get(GetCommentsInput(authUser, slug)).flatMap(toResponse(_))

      case DELETE -> Root / "articles" / slug / "comments" / IntVar(commentId) as authUser =>
        withAuthUser(authUser) { u =>
          comments.delete(DeleteCommentInput(u, slug, commentId)).flatMap(toResponse(_))
        }
    }
  }
}
