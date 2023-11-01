package io.rw.app.routes

import cats.effect.Async
import cats.implicits.*
import io.rw.app.apis.*
import io.rw.app.data.*
import io.rw.app.data.ApiInputs.*
import io.rw.app.data.RequestBodies.*
import io.rw.app.valiation.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.dsl.Http4sDsl
import json.{JsonObject, given}

object ArticleRoutes {

  def apply[F[_]: Async](articles: ArticleApis[F]): AppRoutes[F] = {

    implicit val dsl = Http4sDsl.apply[F]
    import dsl.*

    object Limit extends OptionalQueryParamDecoderMatcher[Int]("limit")
    object Offset extends OptionalQueryParamDecoderMatcher[Int]("offset")

    object Tag extends OptionalQueryParamDecoderMatcher[String]("tag")
    object Author extends OptionalQueryParamDecoderMatcher[String]("author")
    object Favorited
        extends OptionalQueryParamDecoderMatcher[String]("favorited")

    AuthedRoutes.of[Option[AuthUser], F] {
      case GET -> Root / "articles" :? Tag(tag) +& Author(author) +& Favorited(
            favorited
          ) +& Limit(limit) +& Offset(offset) as authUser => {
        val rq = GetAllArticlesInput(
          authUser,
          ArticleFilter(tag, author, favorited),
          Pagination(limit.getOrElse(10), offset.getOrElse(0))
        )
        articles.getAll(rq).map(_.map(JsonCodec.GetArticlesOutput.fromData)).flatMap(toResponse(_))
      }

      case GET -> Root / "articles" / "feed" :? Limit(limit) +& Offset(
            offset
          ) as authUser =>
        withAuthUser(authUser) { u =>
          articles
            .getFeed(
              GetArticlesFeedInput(
                u,
                Pagination(limit.getOrElse(10), offset.getOrElse(0))
              )
            )
            .map(_.map(JsonCodec.GetArticlesOutput.fromData))
            .flatMap(toResponse(_))
        }

      case GET -> Root / "articles" / slug as authUser =>
        articles.get(GetArticleInput(authUser, slug))
          .map(_.map(JsonCodec.WrappedArticle.apply compose JsonCodec.Article.fromData))
          .flatMap(toResponse(_))

      case rq @ POST -> Root / "articles" as authUser =>
        for {
          body <- rq.req.as[JsonCodec.WrappedArticle[JsonCodec.CreateArticle]]
          rs <- withAuthUser(authUser) { u =>
            withValidation(validCreateArticleBody(CreateArticleBody.fromCodec(JsonObject.getSoloValue(body)))) { valid =>
              articles
                .create(
                  CreateArticleInput(
                    u,
                    valid.title,
                    valid.description,
                    valid.body,
                    valid.tagList.getOrElse(List.empty)
                  )
                )
                .map(_.map(JsonCodec.WrappedArticle.apply compose JsonCodec.Article.fromData))
                .flatMap(toResponse(_))
            }
          }
        } yield rs

      case rq @ PUT -> Root / "articles" / slug as authUser =>
        for {
          body <- rq.req.as[JsonCodec.WrappedArticle[JsonCodec.UpdateArticle]]
          rs <- withAuthUser(authUser) { u =>
            withValidation(
              validUpdateArticleBody(
                UpdateArticleBody.fromCodec(JsonObject.getSoloValue(body))
              )
            ) { valid =>
              articles
                .update(
                  UpdateArticleInput(
                    u,
                    slug,
                    valid.title,
                    valid.description,
                    valid.body
                  )
                )
                .map(_.map(JsonCodec.WrappedArticle.apply compose JsonCodec.Article.fromData))
                .flatMap(toResponse(_))
            }
          }
        } yield rs

      case DELETE -> Root / "articles" / slug as authUser =>
        withAuthUser(authUser) { u =>
          articles.delete(DeleteArticleInput(u, slug)).map(_.as(JsonObject.empty)).flatMap(toResponse(_))
        }

      case POST -> Root / "articles" / slug / "favorite" as authUser =>
        withAuthUser(authUser) { u =>
          articles
            .favorite(FavoriteArticleInput(u, slug))
            .map(_.map(JsonCodec.WrappedArticle.apply compose JsonCodec.Article.fromData))
            .flatMap(toResponse(_))
        }

      case DELETE -> Root / "articles" / slug / "favorite" as authUser =>
        withAuthUser(authUser) { u =>
          articles
            .unfavorite(UnfavoriteArticleInput(u, slug))
            .map(_.map(JsonCodec.WrappedArticle.apply compose JsonCodec.Article.fromData))
            .flatMap(toResponse(_))
        }
    }
  }
}
