package test.io.rw.app.routes

import cats._
import cats.data._
import cats.effect.IO
import cats.implicits._
import io.circe.generic.auto._
import io.rw.app.apis._
import io.rw.app.data._
import io.rw.app.data.ApiErrors._
import io.rw.app.data.ApiInputs._
import io.rw.app.data.ApiOutputs._
import io.rw.app.data.RequestBodies._
import io.rw.app.repos._
import io.rw.app.routes._
import io.rw.app.security._
import io.rw.app.utils._
import org.http4s._
import org.http4s.circe.CirceEntityCodec._
import scala.util.Random
import test.io.rw.app.WithEmbededDbTestSuite
import utest._

object CommentRoutesTests extends WithEmbededDbTestSuite {

  val tests = Tests {

    test("add") {
      test("authenticated user should add comment to other's article") {
        val registerBody1 = RegisterUserBody("username1", "email1@email.com", "password123")
        val registerBody2 = RegisterUserBody("username2", "email2@email.com", "password123")
        val createArticleBody = CreateArticleBody("title", "description", "body", Some(List("tag1", "tag2", "tag3")))
        val addCommentBody = AddCommentBody("some comment")

        val t = for {
          jwt1 <- logon(registerBody1)
          rs1 <- postWithToken("articles", WrappedArticleBody(createArticleBody), jwt1)
          slug <- rs1.as[CreateArticleOutput].map(_.article.slug)
          jwt2 <- logon(registerBody2)
          rs2 <- postWithToken(s"articles/$slug/comments", WrappedCommentBody(addCommentBody), jwt2)
          comment <- rs2.as[AddCommentOutput].map(_.comment)
        } yield {
          rs1.status ==> Status.Ok
          rs2.status ==> Status.Ok
          comment.body ==> addCommentBody.body
          comment.author.username ==> registerBody2.username
        }

        t.unsafeRunSync()
      }

      test("authenticated user should add comment to its own article") {
        val registerBody = RegisterUserBody("username", "email@email.com", "password123")
        val createArticleBody = CreateArticleBody("title", "description", "body", Some(List("tag1", "tag2", "tag3")))
        val addCommentBody = AddCommentBody("some comment")

        val t = for {
          jwt <- logon(registerBody)
          rs1 <- postWithToken("articles", WrappedArticleBody(createArticleBody), jwt)
          slug <- rs1.as[CreateArticleOutput].map(_.article.slug)
          rs2 <- postWithToken(s"articles/$slug/comments", WrappedCommentBody(addCommentBody), jwt)
          comment <- rs2.as[AddCommentOutput].map(_.comment)
        } yield {
          rs1.status ==> Status.Ok
          rs2.status ==> Status.Ok
          comment.body ==> addCommentBody.body
          comment.author.username ==> registerBody.username
        }

        t.unsafeRunSync()
      }

      test("authenticated user should add multiple comments article") {
        val registerBody = RegisterUserBody("username", "email@email.com", "password123")
        val createArticleBody = CreateArticleBody("title", "description", "body", Some(List("tag1", "tag2", "tag3")))
        val addCommentBody1 = AddCommentBody("some comment 1")
        val addCommentBody2 = AddCommentBody("some comment 2")

        val t = for {
          jwt <- logon(registerBody)
          rs1 <- postWithToken("articles", WrappedArticleBody(createArticleBody), jwt)
          slug <- rs1.as[CreateArticleOutput].map(_.article.slug)
          rs2 <- postWithToken(s"articles/$slug/comments", WrappedCommentBody(addCommentBody1), jwt)
          comment1 <- rs2.as[AddCommentOutput].map(_.comment)
          rs3 <- postWithToken(s"articles/$slug/comments", WrappedCommentBody(addCommentBody2), jwt)
          comment2 <- rs3.as[AddCommentOutput].map(_.comment)
        } yield {
          rs1.status ==> Status.Ok
          rs2.status ==> Status.Ok
          rs3.status ==> Status.Ok
          comment1.body ==> addCommentBody1.body
          comment1.author.username ==> registerBody.username
          comment2.body ==> addCommentBody2.body
          comment2.author.username ==> registerBody.username
        }

        t.unsafeRunSync()
      }

      test("authenticated user should add comments to different articles") {
        val registerBody = RegisterUserBody("username", "email@email.com", "password123")
        val createArticleBody1 = CreateArticleBody("title1", "description1", "body1", Some(List("tag1", "tag2", "tag3")))
        val createArticleBody2 = CreateArticleBody("title2", "description2", "body2", Some(List("tag1", "tag2", "tag3")))
        val addCommentBody = AddCommentBody("some comment")

        val t = for {
          jwt <- logon(registerBody)
          rs1 <- postWithToken("articles", WrappedArticleBody(createArticleBody1), jwt)
          slug1 <- rs1.as[CreateArticleOutput].map(_.article.slug)
          rs2 <- postWithToken("articles", WrappedArticleBody(createArticleBody1), jwt)
          slug2 <- rs2.as[CreateArticleOutput].map(_.article.slug)
          rs3 <- postWithToken(s"articles/$slug1/comments", WrappedCommentBody(addCommentBody), jwt)
          comment1 <- rs3.as[AddCommentOutput].map(_.comment)
          rs4 <- postWithToken(s"articles/$slug2/comments", WrappedCommentBody(addCommentBody), jwt)
          comment2 <- rs4.as[AddCommentOutput].map(_.comment)
        } yield {
          rs1.status ==> Status.Ok
          rs2.status ==> Status.Ok
          rs3.status ==> Status.Ok
          rs4.status ==> Status.Ok
          comment1.body ==> addCommentBody.body
          comment1.author.username ==> registerBody.username
          comment2.body ==> addCommentBody.body
          comment2.author.username ==> registerBody.username
        }

        t.unsafeRunSync()
      }

      test("authenticated user should get errors when adding comment with invalid body") {
        val registerBody = RegisterUserBody("username", "email@email.com", "password123")
        val createArticleBody = CreateArticleBody("title", "description", "body", Some(List("tag1", "tag2", "tag3")))
        val addCommentBody = AddCommentBody("")

        val t = for {
          jwt <- logon(registerBody)
          rs1 <- postWithToken("articles", WrappedArticleBody(createArticleBody), jwt)
          slug <- rs1.as[CreateArticleOutput].map(_.article.slug)
          rs2 <- postWithToken(s"articles/$slug/comments", WrappedCommentBody(addCommentBody), jwt)
          errors <- rs2.as[ValidationErrorResponse].map(_.errors)
        } yield {
          rs1.status ==> Status.Ok
          rs2.status ==> Status.UnprocessableEntity
          errors.size ==> 1
          errors.get("body") ==> Some(List("can't be blank"))
        }

        t.unsafeRunSync()
      }

      test("authenticated user should get not found when article does not exist") {
        val registerBody = RegisterUserBody("username", "email@email.com", "password123")
        val addCommentBody = AddCommentBody("some comment")

        val t = for {
          jwt <- logon(registerBody)
          rs <- postWithToken("articles/non-existing-slug/comments", WrappedCommentBody(addCommentBody), jwt)
        } yield {
          rs.status ==> Status.NotFound
        }

        t.unsafeRunSync()
      }

      test("not authenticated user should get error") {
        val addCommentBody = AddCommentBody("some comment")

        val t = for {
          rs <- post("articles/slug1/comments", WrappedCommentBody(addCommentBody))
        } yield {
          rs.status ==> Status.Unauthorized
        }

        t.unsafeRunSync()
      }
    }

    test("get") {
      test("authenticated user should get all comments") {
        val registerBody = RegisterUserBody("username", "email@email.com", "password123")
        val createArticleBody = CreateArticleBody("title", "description", "body", Some(List("tag1", "tag2", "tag3")))
        val addCommentBodies = generateAddCommentBodies(10)

        val t = for {
          jwt <- logon(registerBody)
          rs1 <- postWithToken("articles", WrappedArticleBody(createArticleBody), jwt)
          slug <- rs1.as[CreateArticleOutput].map(_.article.slug)
          _ <- addCommentBodies.map(b => postWithToken(s"articles/$slug/comments", WrappedCommentBody(b), jwt)).sequence
          rs2 <- getWithToken(s"articles/$slug/comments", jwt)
          comments <- rs2.as[GetCommentsOutput].map(_.comments)
        } yield {
          rs1.status ==> Status.Ok
          rs2.status ==> Status.Ok
          comments.size ==> addCommentBodies.size
        }

        t.unsafeRunSync()
      }

      test("authenticated user should get zero comments when article is not commented yet") {
        val registerBody = RegisterUserBody("username", "email@email.com", "password123")
        val createArticleBody = CreateArticleBody("title", "description", "body", Some(List("tag1", "tag2", "tag3")))

        val t = for {
          jwt <- logon(registerBody)
          rs1 <- postWithToken("articles", WrappedArticleBody(createArticleBody), jwt)
          slug <- rs1.as[CreateArticleOutput].map(_.article.slug)
          rs2 <- getWithToken(s"articles/$slug/comments", jwt)
          comments <- rs2.as[GetCommentsOutput].map(_.comments)
        } yield {
          rs1.status ==> Status.Ok
          rs2.status ==> Status.Ok
          comments.size ==> 0
        }

        t.unsafeRunSync()
      }

      test("authenticated user should get not found when article does not exist") {
        val registerBody = RegisterUserBody("username", "email@email.com", "password123")

        val t = for {
          jwt <- logon(registerBody)
          rs <- getWithToken(s"articles/not-existing-slug/comments", jwt)
        } yield {
          rs.status ==> Status.NotFound
        }

        t.unsafeRunSync()
      }

      test("not authenticated user should get all comments") {
        val registerBody = RegisterUserBody("username", "email@email.com", "password123")
        val createArticleBody = CreateArticleBody("title", "description", "body", Some(List("tag1", "tag2", "tag3")))
        val addCommentBodies = generateAddCommentBodies(10)

        val t = for {
          jwt <- logon(registerBody)
          rs1 <- postWithToken("articles", WrappedArticleBody(createArticleBody), jwt)
          slug <- rs1.as[CreateArticleOutput].map(_.article.slug)
          _ <- addCommentBodies.map(b => postWithToken(s"articles/$slug/comments", WrappedCommentBody(b), jwt)).sequence
          rs2 <- get(s"articles/$slug/comments")
          comments <- rs2.as[GetCommentsOutput].map(_.comments)
        } yield {
          rs1.status ==> Status.Ok
          rs2.status ==> Status.Ok
          comments.size ==> addCommentBodies.size
        }

        t.unsafeRunSync()
      }
    }

    test("delete") {
      test("authenticated user should delete comment") {
        val registerBody = RegisterUserBody("username", "email@email.com", "password123")
        val createArticleBody = CreateArticleBody("title", "description", "body", Some(List("tag1", "tag2", "tag3")))
        val addCommentBody = AddCommentBody("some comment")

        val t = for {
          jwt <- logon(registerBody)
          rs1 <- postWithToken("articles", WrappedArticleBody(createArticleBody), jwt)
          slug <- rs1.as[CreateArticleOutput].map(_.article.slug)
          rs2 <- postWithToken(s"articles/$slug/comments", WrappedCommentBody(addCommentBody), jwt)
          comment <- rs2.as[AddCommentOutput].map(_.comment)
          rs3 <- deleteWithToken(s"articles/$slug/comments/${comment.id}", jwt)
        } yield {
          rs1.status ==> Status.Ok
          rs2.status ==> Status.Ok
          rs3.status ==> Status.Ok
        }

        t.unsafeRunSync()
      }

      test("authenticated user should get not found when comment does not exist") {
        val registerBody = RegisterUserBody("username", "email@email.com", "password123")
        val createArticleBody = CreateArticleBody("title", "description", "body", Some(List("tag1", "tag2", "tag3")))

        val t = for {
          jwt <- logon(registerBody)
          rs1 <- postWithToken("articles", WrappedArticleBody(createArticleBody), jwt)
          slug <- rs1.as[CreateArticleOutput].map(_.article.slug)
          rs2 <- deleteWithToken(s"articles/$slug/comments/123", jwt)
        } yield {
          rs1.status ==> Status.Ok
          rs2.status ==> Status.NotFound
        }

        t.unsafeRunSync()
      }

      test("authenticated user should get not found when deleting another's comment") {
        val registerBody1 = RegisterUserBody("username1", "email1@email.com", "password123")
        val registerBody2 = RegisterUserBody("username2", "email2@email.com", "password123")
        val createArticleBody = CreateArticleBody("title", "description", "body", Some(List("tag1", "tag2", "tag3")))
        val addCommentBody = AddCommentBody("some comment")

        val t = for {
          jwt1 <- logon(registerBody1)
          rs1 <- postWithToken("articles", WrappedArticleBody(createArticleBody), jwt1)
          slug <- rs1.as[CreateArticleOutput].map(_.article.slug)
          rs2 <- postWithToken(s"articles/$slug/comments", WrappedCommentBody(addCommentBody), jwt1)
          comment <- rs2.as[AddCommentOutput].map(_.comment)
          jwt2 <- logon(registerBody2)
          rs3 <- deleteWithToken(s"articles/$slug/comments/${comment.id}", jwt2)
        } yield {
          rs1.status ==> Status.Ok
          rs2.status ==> Status.Ok
          rs3.status ==> Status.NotFound
        }

        t.unsafeRunSync()
      }

      test("authenticated user should get not found when article does not exist") {
        val registerBody = RegisterUserBody("username", "email@email.com", "password123")

        val t = for {
          jwt <- logon(registerBody)
          rs <- deleteWithToken("articles/non-existing-slug/comments/123", jwt)
        } yield {
          rs.status ==> Status.NotFound
        }

        t.unsafeRunSync()
      }

      test("not authenticated user should get error") {
        val t = for {
          rs <- delete(s"articles/not-existing-slug/comments/1")
        } yield {
          rs.status ==> Status.Unauthorized
        }

        t.unsafeRunSync()
      }
    }

    def generateAddCommentBodies(n: Int, tags: List[String] = List.empty): List[AddCommentBody] =
      List.fill(n) {
        val body = Random.shuffle("comment").mkString
        AddCommentBody(body)
      }

    implicit val app: HttpApp[IO] = {
      val passwordHasher = PasswordHasher.impl
      val idHasher = IdHasher.impl("salt")
      val userRepo = UserRepo.impl(xa)
      val articleRepo = ArticleRepo.impl(xa)
      val followerRepo = FollowerRepo.impl(xa)
      val tagRepo = TagRepo.impl(xa)
      val favoriteRepo = FavoriteRepo.impl(xa)
      val commentRepo = CommentRepo.impl(xa)
      val userApis = UserApis.impl(passwordHasher, token, userRepo)
      val articleApis = ArticleApis.impl(articleRepo, followerRepo, tagRepo, favoriteRepo, idHasher)
      val commentApis = CommentApis.impl(commentRepo, articleRepo, followerRepo)
      mkApp(List(UserRoutes(userApis), ArticleRoutes(articleApis), CommentRoutes(commentApis)))
    }
  }
}
