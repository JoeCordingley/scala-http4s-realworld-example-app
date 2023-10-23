package test.io.rw.app.routes

import cats.*
import cats.data.*
import cats.effect.IO
import cats.implicits.*
import io.circe.generic.auto.*
import io.rw.app.apis.*
import io.rw.app.data.*
import io.rw.app.data.ApiErrors.*
import io.rw.app.data.ApiInputs.*
import io.rw.app.data.ApiOutputs.*
import io.rw.app.data.RequestBodies.*
import io.rw.app.repos.*
import io.rw.app.routes.*
import io.rw.app.security.*
import io.rw.app.utils.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import scala.util.Random
import test.io.rw.app.WithEmbededDbTestSuite
import utest.*
import doobie.util.update
import io.rw.app.utils
import cats.effect.unsafe.implicits.global

object ArticleRoutesTests extends WithEmbededDbTestSuite {

  val tests = Tests {
    test("get all") {
      test("authenticated user should get all articles") {
        val registerBody =
          registerUserBody("username", "email@email.com", "password123")
        val createArticleBodies = generateCreateArticleBodies(10)

        val t = for {
          jwt <- logon(registerBody)
          _ <- createArticleBodies
            .map(b => postWithToken("articles", WrappedArticleBody(b), jwt))
            .sequence
          rs <- getWithToken("articles", jwt)
          (articles, articlesCount) <- rs
            .as[GetAllArticlesOutput]
            .map(r => (r.articles, r.articlesCount))
        } yield {
          rs.status ==> Status.Ok
          articlesCount ==> createArticleBodies.size
        }

        t.unsafeRunSync()
      }

      test("authenticated user should get articles paginated") {
        val registerBody =
          registerUserBody("username", "email@email.com", "password123")
        val createArticleBodies = generateCreateArticleBodies(10)

        val t = for {
          jwt <- logon(registerBody)
          _ <- createArticleBodies
            .map(b => postWithToken("articles", WrappedArticleBody(b), jwt))
            .sequence
          rs1 <- getWithToken(s"articles?limit=7&offset=0", jwt)
          (articles1, articlesCount1) <- rs1
            .as[GetAllArticlesOutput]
            .map(r => (r.articles, r.articlesCount))
          rs2 <- getWithToken(s"articles?limit=7&offset=7", jwt)
          (articles2, articlesCount2) <- rs2
            .as[GetAllArticlesOutput]
            .map(r => (r.articles, r.articlesCount))
          rs3 <- getWithToken(s"articles?limit=7&offset=10", jwt)
          (articles3, articlesCount3) <- rs3
            .as[GetAllArticlesOutput]
            .map(r => (r.articles, r.articlesCount))
        } yield {
          rs1.status ==> Status.Ok
          rs2.status ==> Status.Ok
          rs3.status ==> Status.Ok
          articles1.size ==> 7
          articlesCount1 ==> createArticleBodies.size
          articles2.size ==> 3
          articlesCount2 ==> createArticleBodies.size
          articles3.size ==> 0
          articlesCount3 ==> createArticleBodies.size
        }

        t.unsafeRunSync()
      }

      test("authenticated user should get articles by tag") {
        val registerBody =
          registerUserBody("username", "email@email.com", "password123")
        val createArticleBodies = generateCreateArticleBodies(10)
        val tag = "tagAbc"
        val createArticleBodiesWithTag =
          generateCreateArticleBodies(5, List(tag))

        val t = for {
          jwt <- logon(registerBody)
          _ <- (createArticleBodies ++ createArticleBodiesWithTag)
            .map(b => postWithToken("articles", WrappedArticleBody(b), jwt))
            .sequence
          rs <- getWithToken(s"articles?tag=$tag", jwt)
          (articles, articlesCount) <- rs
            .as[GetAllArticlesOutput]
            .map(r => (r.articles, r.articlesCount))
        } yield {
          rs.status ==> Status.Ok
          articlesCount ==> createArticleBodiesWithTag.size
        }

        t.unsafeRunSync()
      }

      test("authenticated user should get articles by author") {
        val registerBody1 =
          registerUserBody("username1", "email1@email.com", "password123")
        val username2 = "username2"
        val registerBody2 =
          registerUserBody(username2, "email2@email.com", "password123")
        val createArticleBodies1 = generateCreateArticleBodies(10)
        val createArticleBodies2 = generateCreateArticleBodies(7)

        val t = for {
          jwt1 <- logon(registerBody1)
          _ <- createArticleBodies1
            .map(b => postWithToken("articles", WrappedArticleBody(b), jwt1))
            .sequence
          jwt2 <- logon(registerBody2)
          _ <- createArticleBodies2
            .map(b => postWithToken("articles", WrappedArticleBody(b), jwt2))
            .sequence
          rs <- getWithToken(s"articles?author=$username2", jwt1)
          (articles, articlesCount) <- rs
            .as[GetAllArticlesOutput]
            .map(r => (r.articles, r.articlesCount))
        } yield {
          rs.status ==> Status.Ok
          articlesCount ==> createArticleBodies2.size
        }

        t.unsafeRunSync()
      }

      test("authenticated user should get articles by favorited") {
        val username1 = "username1"
        val registerBody1 =
          registerUserBody(username1, "email1@email.com", "password123")
        val username2 = "username2"
        val registerBody2 =
          registerUserBody(username2, "email2@email.com", "password123")
        val createArticleBodies1 = generateCreateArticleBodies(10)
        val createArticleBodies2 = generateCreateArticleBodies(7)

        val t = for {
          jwt1 <- logon(registerBody1)
          _ <- createArticleBodies1
            .map(b => postWithToken("articles", WrappedArticleBody(b), jwt1))
            .sequence
          jwt2 <- logon(registerBody2)
          _ <- createArticleBodies2
            .map(b => postWithToken("articles", WrappedArticleBody(b), jwt2))
            .sequence
          rs1 <- getWithToken(
            s"articles?author=$username2",
            jwt1
          )
          (articles, articlesCount) <- rs1
            .as[GetAllArticlesOutput]
            .map(r => (r.articles, r.articlesCount))
          favorites = articles.take(5)
          _ <- favorites
            .map(a => postWithToken(s"articles/${a.slug}/favorite", jwt1))
            .sequence
          rs2 <- getWithToken(
            s"articles?favorited=$username1",
            jwt1
          )
          (articles2, articlesCount2) <- rs2
            .as[GetAllArticlesOutput]
            .map(r => (r.articles, r.articlesCount))
        } yield {
          rs1.status ==> Status.Ok
          rs2.status ==> Status.Ok
          articles2.filter(_.favorited).size ==> favorites.size
          articles2.filter(_.favoritesCount == 1).size ==> favorites.size
          articlesCount2 ==> favorites.size
        }

        t.unsafeRunSync()
      }

      test("not authenticated user should get all articles") {
        val registerBody =
          registerUserBody("username", "email@email.com", "password123")
        val createArticleBodies = generateCreateArticleBodies(10)

        val t = for {
          jwt <- logon(registerBody)
          _ <- createArticleBodies
            .map(b => postWithToken("articles", WrappedArticleBody(b), jwt))
            .sequence
          rs <- get("articles")
          (articles, articlesCount) <- rs
            .as[GetAllArticlesOutput]
            .map(r => (r.articles, r.articlesCount))
        } yield {
          rs.status ==> Status.Ok
          articlesCount ==> createArticleBodies.size
        }

        t.unsafeRunSync()
      }
    }

    test("get feed") {
      test("authenticated user should get feed") {
        val registerBody1 =
          registerUserBody("username1", "email1@email.com", "password123")
        val username2 = "username2"
        val registerBody2 =
          registerUserBody(username2, "email2@email.com", "password123")
        val createArticleBodies1 = generateCreateArticleBodies(10)
        val createArticleBodies2 = generateCreateArticleBodies(7)

        val t = for {
          jwt1 <- logon(registerBody1)
          _ <- createArticleBodies1
            .map(b => postWithToken("articles", WrappedArticleBody(b), jwt1))
            .sequence
          jwt2 <- logon(registerBody2)
          _ <- createArticleBodies2
            .map(b => postWithToken("articles", WrappedArticleBody(b), jwt2))
            .sequence
          rs1 <- postWithToken(
            s"profiles/$username2/follow",
            jwt1
          )
          rs2 <- getWithToken(s"articles/feed", jwt1)
          (articles, articlesCount) <- rs2
            .as[GetArticlesFeedOutput]
            .map(r => (r.articles, r.articlesCount))
        } yield {
          rs1.status ==> Status.Ok
          rs2.status ==> Status.Ok
          articlesCount ==> createArticleBodies2.size
        }

        t.unsafeRunSync()
      }

      test("authenticated user should get feed paginated") {
        val registerBody1 =
          registerUserBody("username1", "email1@email.com", "password123")
        val username2 = "username2"
        val registerBody2 =
          registerUserBody(username2, "email2@email.com", "password123")
        val createArticleBodies1 = generateCreateArticleBodies(10)
        val createArticleBodies2 = generateCreateArticleBodies(10)

        val t = for {
          jwt1 <- logon(registerBody1)
          _ <- createArticleBodies1
            .map(b => postWithToken("articles", WrappedArticleBody(b), jwt1))
            .sequence
          jwt2 <- logon(registerBody2)
          _ <- createArticleBodies2
            .map(b => postWithToken("articles", WrappedArticleBody(b), jwt2))
            .sequence
          rs1 <- postWithToken(
            s"profiles/$username2/follow",
            jwt1
          )
          rs2 <- getWithToken(s"articles/feed?limit=7&offset=0", jwt1)
          (articles1, articlesCount1) <- rs2
            .as[GetArticlesFeedOutput]
            .map(r => (r.articles, r.articlesCount))
          rs3 <- getWithToken(s"articles/feed?limit=7&offset=7", jwt1)
          (articles2, articlesCount2) <- rs3
            .as[GetArticlesFeedOutput]
            .map(r => (r.articles, r.articlesCount))
          rs4 <- getWithToken(s"articles/feed?limit=7&offset=10", jwt1)
          (articles3, articlesCount3) <- rs4
            .as[GetArticlesFeedOutput]
            .map(r => (r.articles, r.articlesCount))
        } yield {
          rs1.status ==> Status.Ok
          rs2.status ==> Status.Ok
          rs3.status ==> Status.Ok
          rs4.status ==> Status.Ok
          articles1.size ==> 7
          articlesCount1 ==> createArticleBodies2.size
          articles2.size ==> 3
          articlesCount2 ==> createArticleBodies2.size
          articles3.size ==> 0
          articlesCount3 ==> createArticleBodies2.size
        }

        t.unsafeRunSync()
      }

      test("not authenticated user should get error") {
        val t = for {
          rs <- get("articles/feed")
        } yield {
          rs.status ==> Status.Unauthorized
        }

        t.unsafeRunSync()
      }
    }

    test("get") {
      test("authenticated user should get article by slug") {
        val registerBody =
          registerUserBody("username", "email@email.com", "password123")
        val createArticleBody = CreateArticleBody(
          "title",
          "description",
          "body",
          Some(List("tag1", "tag2", "tag3"))
        )

        val t = for {
          jwt <- logon(registerBody)
          rs1 <- postWithToken(
            "articles",
            WrappedArticleBody(createArticleBody),
            jwt
          )
          slug <- rs1.as[CreateArticleOutput].map(r => r.article.slug)
          rs2 <- getWithToken(s"articles/$slug", jwt)
        } yield {
          rs2.status ==> Status.Ok
        }

        t.unsafeRunSync()
      }

      test(
        "authenticated user should get not found when article does not exist"
      ) {
        val registerBody =
          registerUserBody("username", "email@email.com", "password123")

        val t = for {
          jwt <- logon(registerBody)
          rs <- getWithToken("articles/slug-not-exists", jwt)
        } yield {
          rs.status ==> Status.NotFound
        }

        t.unsafeRunSync()
      }

      test("not authenticated user should get article by slug") {
        val registerBody =
          registerUserBody("username", "email@email.com", "password123")
        val createArticleBody = CreateArticleBody(
          "title",
          "description",
          "body",
          Some(List("tag1", "tag2", "tag3"))
        )

        val t = for {
          jwt <- logon(registerBody)
          rs1 <- postWithToken(
            "articles",
            WrappedArticleBody(createArticleBody),
            jwt
          )
          slug <- rs1.as[CreateArticleOutput].map(r => r.article.slug)
          rs2 <- get(s"articles/$slug")
        } yield {
          rs2.status ==> Status.Ok
        }

        t.unsafeRunSync()
      }
    }

    test("create") {
      test("authenticated user should create article") {
        val username = "username"
        val registerBody =
          registerUserBody(username, "email@email.com", "password123")
        val createArticleBody = CreateArticleBody(
          "title",
          "description",
          "body",
          Some(List("tag1", "tag2", "tag3"))
        )

        val t = for {
          jwt <- logon(registerBody)
          rs <- postWithToken(
            "articles",
            WrappedArticleBody(createArticleBody),
            jwt
          )
          article <- rs.as[CreateArticleOutput].map(_.article)
        } yield {
          rs.status ==> Status.Ok
          article.title ==> createArticleBody.title
          article.description ==> createArticleBody.description
          article.body ==> createArticleBody.body
          article.favoritesCount ==> 0
          article.favorited ==> false
          Some(article.tagList.toSet) ==> createArticleBody.tagList.map(_.toSet)
          article.author.username ==> username
        }

        t.unsafeRunSync()
      }

      test("authenticated user should create article with the same title") {
        val username = "username"
        val registerBody =
          registerUserBody(username, "email@email.com", "password123")
        val createArticleBody1 = CreateArticleBody(
          "title",
          "description1",
          "body1",
          Some(List("tag1", "tag2", "tag3"))
        )
        val createArticleBody2 = CreateArticleBody(
          "title",
          "description2",
          "body2",
          Some(List("tag1", "tag2", "tag3"))
        )

        val t = for {
          jwt <- logon(registerBody)
          rs1 <- postWithToken(
            "articles",
            WrappedArticleBody(createArticleBody1),
            jwt
          )
          article1 <- rs1.as[CreateArticleOutput].map(_.article)
          rs2 <- postWithToken(
            "articles",
            WrappedArticleBody(createArticleBody2),
            jwt
          )
          article2 <- rs2.as[CreateArticleOutput].map(_.article)
        } yield {
          rs1.status ==> Status.Ok
          article1.title ==> createArticleBody1.title
          article1.description ==> createArticleBody1.description
          article1.body ==> createArticleBody1.body
          article1.favoritesCount ==> 0
          article1.favorited ==> false
          Some(article1.tagList.toSet) ==> createArticleBody1.tagList.map(
            _.toSet
          )
          article1.author.username ==> username
          rs2.status ==> Status.Ok
          article2.title ==> createArticleBody2.title
          article2.description ==> createArticleBody2.description
          article2.body ==> createArticleBody2.body
          article2.favoritesCount ==> 0
          article2.favorited ==> false
          Some(article2.tagList.toSet) ==> createArticleBody2.tagList.map(
            _.toSet
          )
          article2.author.username ==> username
        }

        t.unsafeRunSync()
      }

      test(
        "authenticated user should get errors when creating article with invalid title, body or description"
      ) {
        val registerBody =
          registerUserBody("username", "email@email.com", "password123")
        val createArticleBody =
          CreateArticleBody("", "", "", Some(List("tag1", "tag2", "tag3")))

        val t = for {
          jwt <- logon(registerBody)
          rs <- postWithToken(
            "articles",
            WrappedArticleBody(createArticleBody),
            jwt
          )
          errors <- rs.as[ValidationErrorResponse].map(_.errors)
        } yield {
          rs.status ==> Status.UnprocessableEntity
          errors.size ==> 3
          errors.get("title") ==> Some(List("can't be blank"))
          errors.get("body") ==> Some(List("can't be blank"))
          errors.get("description") ==> Some(List("can't be blank"))
        }

        t.unsafeRunSync()
      }

      test("not authenticated user should get error") {
        val createArticleBody = CreateArticleBody(
          "title",
          "description",
          "body",
          Some(List("tag1", "tag2", "tag3"))
        )

        val t = for {
          rs <- post("articles", WrappedArticleBody(createArticleBody))
        } yield {
          rs.status ==> Status.Unauthorized
        }

        t.unsafeRunSync()
      }
    }

    test("update") {
      test("authenticated user should update article") {
        val username = "username"
        val registerBody =
          registerUserBody(username, "email@email.com", "password123")
        val createArticleBody = CreateArticleBody(
          "title",
          "description",
          "body",
          Some(List("tag1", "tag2", "tag3"))
        )
        val updateArticleBody = UpdateArticleBody(
          Some("newTitle"),
          Some("newDescription"),
          Some("newBody")
        )

        val t = for {
          jwt <- logon(registerBody)
          rs1 <- postWithToken(
            "articles",
            WrappedArticleBody(createArticleBody),
            jwt
          )
          slug <- rs1.as[CreateArticleOutput].map(_.article.slug)
          rs2 <- putWithToken(
            s"articles/$slug",
            WrappedArticleBody(updateArticleBody),
            jwt
          )
          article <- rs2.as[UpdateArticleOutput].map(_.article)
        } yield {
          rs1.status ==> Status.Ok
          rs2.status ==> Status.Ok
          Some(article.title) ==> updateArticleBody.title
          Some(article.description) ==> updateArticleBody.description
          Some(article.body) ==> updateArticleBody.body
          article.favoritesCount ==> 0
          article.favorited ==> false
          Some(article.tagList.toSet) ==> createArticleBody.tagList.map(_.toSet)
          article.author.username ==> username
        }

        t.unsafeRunSync()
      }

      test(
        "authenticated user should get errors when updating article with invalid title, body or description"
      ) {
        val registerBody =
          registerUserBody("username", "email@email.com", "password123")
        val createArticleBody = CreateArticleBody(
          "title",
          "description",
          "body",
          Some(List("tag1", "tag2", "tag3"))
        )
        val updateArticleBody = UpdateArticleBody(Some(""), Some(""), Some(""))

        val t = for {
          jwt <- logon(registerBody)
          rs1 <- postWithToken(
            "articles",
            WrappedArticleBody(createArticleBody),
            jwt
          )
          slug <- rs1.as[CreateArticleOutput].map(_.article.slug)
          rs2 <- putWithToken(
            s"articles/$slug",
            WrappedArticleBody(updateArticleBody),
            jwt
          )
          errors <- rs2.as[ValidationErrorResponse].map(_.errors)
        } yield {
          rs1.status ==> Status.Ok
          rs2.status ==> Status.UnprocessableEntity
          errors.size ==> 3
          errors.get("title") ==> Some(List("can't be blank"))
          errors.get("body") ==> Some(List("can't be blank"))
          errors.get("description") ==> Some(List("can't be blank"))
        }

        t.unsafeRunSync()
      }

      test(
        "authenticated user should get not found when updating non existing article"
      ) {
        val registerBody =
          registerUserBody("username", "email@email.com", "password123")
        val updateArticleBody = UpdateArticleBody(
          Some("newTitle"),
          Some("newDescription"),
          Some("newBody")
        )

        val t = for {
          jwt <- logon(registerBody)
          rs <- putWithToken(
            "articles/non-existing-slug",
            WrappedArticleBody(updateArticleBody),
            jwt
          )
        } yield {
          rs.status ==> Status.NotFound
        }

        t.unsafeRunSync()
      }

      test(
        "authenticated user should get not found when updating another's article"
      ) {
        val registerBody1 =
          registerUserBody("username1", "email1@email.com", "password123")
        val registerBody2 =
          registerUserBody("username2", "email2@email.com", "password123")
        val createArticleBody = CreateArticleBody(
          "title",
          "description",
          "body",
          Some(List("tag1", "tag2", "tag3"))
        )
        val updateArticleBody = UpdateArticleBody(
          Some("newTitle"),
          Some("newDescription"),
          Some("newBody")
        )

        val t = for {
          jwt1 <- logon(registerBody1)
          rs1 <- postWithToken(
            "articles",
            WrappedArticleBody(createArticleBody),
            jwt1
          )
          slug <- rs1.as[CreateArticleOutput].map(_.article.slug)
          jwt2 <- logon(registerBody2)
          rs2 <- putWithToken(
            s"articles/$slug",
            WrappedArticleBody(updateArticleBody),
            jwt2
          )
        } yield {
          rs1.status ==> Status.Ok
          rs2.status ==> Status.NotFound
        }

        t.unsafeRunSync()
      }

      test("not authenticated user should get error") {
        val updateArticleBody =
          UpdateArticleBody(Some("newTitle"), Some(""), Some(""))

        val t = for {
          rs <- put("articles/slug", WrappedArticleBody(updateArticleBody))
        } yield {
          rs.status ==> Status.Unauthorized
        }

        t.unsafeRunSync()
      }
    }

    test("delete") {
      test("authenticated user should delete article") {
        val registerBody =
          registerUserBody("username", "email@email.com", "password123")
        val createArticleBody = CreateArticleBody(
          "title",
          "description",
          "body",
          Some(List("tag1", "tag2", "tag3"))
        )

        val t = for {
          jwt <- logon(registerBody)
          rs1 <- postWithToken(
            "articles",
            WrappedArticleBody(createArticleBody),
            jwt
          )
          slug <- rs1.as[CreateArticleOutput].map(_.article.slug)
          rs2 <- deleteWithToken(s"articles/$slug", jwt)
          rs3 <- getWithToken(s"articles/$slug", jwt)
        } yield {
          rs1.status ==> Status.Ok
          rs2.status ==> Status.Ok
          rs3.status ==> Status.NotFound
        }

        t.unsafeRunSync()
      }

      test(
        "authenticated user should get not found when article does not exist"
      ) {
        val registerBody =
          registerUserBody("username", "email@email.com", "password123")

        val t = for {
          jwt <- logon(registerBody)
          rs <- deleteWithToken("articles/non-exising-slug", jwt)
        } yield {
          rs.status ==> Status.NotFound
        }

        t.unsafeRunSync()
      }

      test(
        "authenticated user should get not found when deleting another's article"
      ) {
        val registerBody1 =
          registerUserBody("username1", "email1@email.com", "password123")
        val registerBody2 =
          registerUserBody("username2", "email2@email.com", "password123")
        val createArticleBody = CreateArticleBody(
          "title",
          "description",
          "body",
          Some(List("tag1", "tag2", "tag3"))
        )

        val t = for {
          jwt1 <- logon(registerBody1)
          rs1 <- postWithToken(
            "articles",
            WrappedArticleBody(createArticleBody),
            jwt1
          )
          slug <- rs1.as[CreateArticleOutput].map(_.article.slug)
          jwt2 <- logon(registerBody2)
          rs2 <- deleteWithToken(s"articles/$slug", jwt2)
          rs3 <- getWithToken(s"articles/$slug", jwt1)
        } yield {
          rs1.status ==> Status.Ok
          rs2.status ==> Status.NotFound
          rs3.status ==> Status.Ok
        }

        t.unsafeRunSync()
      }

      test("not authenticated user should get error") {
        val t = for {
          rs <- delete("articles/slug")
        } yield {
          rs.status ==> Status.Unauthorized
        }

        t.unsafeRunSync()
      }
    }

    test("favorite") {
      test("authenticated user should favorite article") {
        val registerBody1 =
          registerUserBody("username1", "email1@email.com", "password123")
        val registerBody2 =
          registerUserBody("username2", "email2@email.com", "password123")
        val registerBody3 =
          registerUserBody("username3", "email3@email.com", "password123")
        val createArticleBody = CreateArticleBody(
          "title",
          "description",
          "body",
          Some(List("tag1", "tag2", "tag3"))
        )

        val t = for {
          jwt1 <- logon(registerBody1)
          rs1 <- postWithToken(
            "articles",
            WrappedArticleBody(createArticleBody),
            jwt1
          )
          slug <- rs1.as[CreateArticleOutput].map(_.article.slug)
          jwt2 <- logon(registerBody2)
          rs2 <- postWithToken(s"articles/$slug/favorite", jwt2)
          article1 <- rs2.as[FavoriteArticleOutput].map(_.article)
          jwt3 <- logon(registerBody3)
          rs3 <- postWithToken(s"articles/$slug/favorite", jwt3)
          article2 <- rs3.as[FavoriteArticleOutput].map(_.article)
        } yield {
          rs1.status ==> Status.Ok
          rs2.status ==> Status.Ok
          rs3.status ==> Status.Ok
          article1.favoritesCount ==> 1
          article1.favorited ==> true
          article2.favoritesCount ==> 2
          article2.favorited ==> true
        }

        t.unsafeRunSync()
      }

      test("authenticated user should favorite article twice") {
        val registerBody1 =
          registerUserBody("username1", "email1@email.com", "password123")
        val registerBody2 =
          registerUserBody("username2", "email2@email.com", "password123")
        val createArticleBody = CreateArticleBody(
          "title",
          "description",
          "body",
          Some(List("tag1", "tag2", "tag3"))
        )

        val t = for {
          jwt1 <- logon(registerBody1)
          rs1 <- postWithToken(
            "articles",
            WrappedArticleBody(createArticleBody),
            jwt1
          )
          slug <- rs1.as[CreateArticleOutput].map(_.article.slug)
          jwt2 <- logon(registerBody2)
          rs2 <- postWithToken(s"articles/$slug/favorite", jwt2)
          article1 <- rs2.as[FavoriteArticleOutput].map(_.article)
          rs3 <- postWithToken(s"articles/$slug/favorite", jwt2)
          article2 <- rs3.as[FavoriteArticleOutput].map(_.article)
        } yield {
          rs1.status ==> Status.Ok
          rs2.status ==> Status.Ok
          rs3.status ==> Status.Ok
          article1.favoritesCount ==> 1
          article1.favorited ==> true
          article2.favoritesCount ==> 1
          article2.favorited ==> true
        }

        t.unsafeRunSync()
      }

      test(
        "authenticated user should get not found when article does not exist"
      ) {
        val registerBody =
          registerUserBody("username", "email@email.com", "password123")

        val t = for {
          jwt <- logon(registerBody)
          rs <- postWithToken(s"articles/non-existing-slug/favorite", jwt)
        } yield {
          rs.status ==> Status.NotFound
        }

        t.unsafeRunSync()
      }

      test("not authenticated user should get error") {
        val t = for {
          rs <- post(s"articles/non-existing-slug/favorite")
        } yield {
          rs.status ==> Status.Unauthorized
        }

        t.unsafeRunSync()
      }
    }

    test("unfavorite") {
      test("authenticated user should unfavorite article") {
        val registerBody1 =
          registerUserBody("username1", "email1@email.com", "password123")
        val registerBody2 =
          registerUserBody("username2", "email2@email.com", "password123")
        val createArticleBody = CreateArticleBody(
          "title",
          "description",
          "body",
          Some(List("tag1", "tag2", "tag3"))
        )

        val t = for {
          jwt1 <- logon(registerBody1)
          rs1 <- postWithToken(
            "articles",
            WrappedArticleBody(createArticleBody),
            jwt1
          )
          slug <- rs1.as[CreateArticleOutput].map(_.article.slug)
          jwt2 <- logon(registerBody2)
          rs2 <- postWithToken(s"articles/$slug/favorite", jwt2)
          article1 <- rs2.as[UnfavoriteArticleOutput].map(_.article)
          rs3 <- deleteWithToken(s"articles/$slug/favorite", jwt2)
          article2 <- rs3.as[UnfavoriteArticleOutput].map(_.article)
        } yield {
          rs1.status ==> Status.Ok
          rs2.status ==> Status.Ok
          rs3.status ==> Status.Ok
          article1.favoritesCount ==> 1
          article1.favorited ==> true
          article2.favoritesCount ==> 0
          article2.favorited ==> false
        }

        t.unsafeRunSync()
      }

      test("authenticated user should unfavorite article twice") {
        val registerBody1 =
          registerUserBody("username1", "email1@email.com", "password123")
        val registerBody2 =
          registerUserBody("username2", "email2@email.com", "password123")
        val createArticleBody = CreateArticleBody(
          "title",
          "description",
          "body",
          Some(List("tag1", "tag2", "tag3"))
        )

        val t = for {
          jwt1 <- logon(registerBody1)
          rs1 <- postWithToken(
            "articles",
            WrappedArticleBody(createArticleBody),
            jwt1
          )
          slug <- rs1.as[CreateArticleOutput].map(_.article.slug)
          jwt2 <- logon(registerBody2)
          rs2 <- postWithToken(s"articles/$slug/favorite", jwt2)
          article1 <- rs2.as[UnfavoriteArticleOutput].map(_.article)
          rs3 <- deleteWithToken(s"articles/$slug/favorite", jwt2)
          article2 <- rs3.as[UnfavoriteArticleOutput].map(_.article)
          rs4 <- deleteWithToken(s"articles/$slug/favorite", jwt2)
          article3 <- rs4.as[UnfavoriteArticleOutput].map(_.article)
        } yield {
          rs1.status ==> Status.Ok
          rs2.status ==> Status.Ok
          rs3.status ==> Status.Ok
          rs4.status ==> Status.Ok
          article1.favoritesCount ==> 1
          article1.favorited ==> true
          article2.favoritesCount ==> 0
          article2.favorited ==> false
          article3.favoritesCount ==> 0
          article3.favorited ==> false
        }

        t.unsafeRunSync()
      }

      test(
        "authenticated user should get not found when article does not exist"
      ) {
        val registerBody =
          registerUserBody("username", "email@email.com", "password123")

        val t = for {
          jwt <- logon(registerBody)
          rs <- deleteWithToken(s"articles/non-existing-slug/favorite", jwt)
        } yield {
          rs.status ==> Status.NotFound
        }

        t.unsafeRunSync()
      }

      test("not authenticated user should get error") {
        val t = for {
          rs <- delete(s"articles/non-existing-slug/favorite")
        } yield {
          rs.status ==> Status.Unauthorized
        }

        t.unsafeRunSync()
      }
    }
  }

  def generateCreateArticleBodies(
      n: Int,
      tags: List[String] = List.empty
  ): List[CreateArticleBody] =
    List.fill(n) {
      val title = Random.shuffle("articletitle").mkString
      val description = Random.shuffle("articledescription").mkString
      val body = Random.shuffle("articlebody").mkString
      val tagList =
        if (tags.isEmpty)
          List("tag1", "tag2", "tag3", "tag4").map(Random.shuffle(_).mkString)
        else tags
      CreateArticleBody(title, description, body, Some(tagList))
    }

  implicit val app: HttpApp[IO] = {
    val passwordHasher = PasswordHasher.impl
    val idHasher = IdHasher.impl("salt")
    val userRepo = UserRepo.impl(xa)
    val articleRepo = ArticleRepo.impl(xa)
    val followerRepo = FollowerRepo.impl(xa)
    val tagRepo = TagRepo.impl(xa)
    val favoriteRepo = FavoriteRepo.impl(xa)
    val userApis = UserApis.impl(passwordHasher, token, userRepo)
    val profileApis = ProfileApis.impl(userRepo, followerRepo)
    val articleApis = ArticleApis.impl(
      articleRepo,
      followerRepo,
      tagRepo,
      favoriteRepo,
      idHasher
    )
    mkApp(
      List(
        UserRoutes(userApis),
        ProfileRoutes(profileApis),
        ArticleRoutes(articleApis)
      )
    )
  }
}
