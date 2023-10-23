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
import cats.effect.unsafe.implicits.global

object TagRoutesTests extends WithEmbededDbTestSuite {

  val tests = Tests {

    test("get") {
      test("authenticated user should get all tags") {
        val registerBody =
          registerUserBody("username", "email@email.com", "password123")
        val mostPopularTag = "tagX"
        val tags1 = List(mostPopularTag, "tag1", "tag2", "tag3", "tag4")
        val tags2 = List("tag2", mostPopularTag, "tag4", "tag5", "tag6")
        val tags3 = List("tag7", "tag5", mostPopularTag)
        val createArticleBody1 =
          CreateArticleBody("title1", "description", "body", Some(tags1))
        val createArticleBody2 =
          CreateArticleBody("title2", "description", "body", Some(tags2))
        val createArticleBody3 =
          CreateArticleBody("title3", "description", "body", Some(tags3))

        val t = for {
          jwt <- logon(registerBody)
          rs1 <- postWithToken(
            "articles",
            WrappedArticleBody(createArticleBody1),
            jwt
          )
          rs2 <- postWithToken(
            "articles",
            WrappedArticleBody(createArticleBody2),
            jwt
          )
          rs3 <- postWithToken(
            "articles",
            WrappedArticleBody(createArticleBody3),
            jwt
          )
          rs4 <- getWithToken("tags", jwt)
          tags <- rs4.as[GetTagsOutput].map(_.tags)
        } yield {
          rs1.status ==> Status.Ok
          rs2.status ==> Status.Ok
          rs3.status ==> Status.Ok
          rs4.status ==> Status.Ok
          tags.size ==> (tags1 ++ tags2 ++ tags3).toSet.size
          tags.head ==> mostPopularTag
        }

        t.unsafeRunSync()
      }

      test("non authenticated user should get all tags") {
        val registerBody =
          registerUserBody("username", "email@email.com", "password123")
        val mostPopularTag = "tagX"
        val tags1 = List(mostPopularTag, "tag1", "tag2", "tag3", "tag4")
        val tags2 = List("tag2", mostPopularTag, "tag4", "tag5", "tag6")
        val tags3 = List("tag7", "tag5", mostPopularTag)
        val createArticleBody1 =
          CreateArticleBody("title1", "description", "body", Some(tags1))
        val createArticleBody2 =
          CreateArticleBody("title2", "description", "body", Some(tags2))
        val createArticleBody3 =
          CreateArticleBody("title3", "description", "body", Some(tags3))

        val t = for {
          jwt <- logon(registerBody)
          rs1 <- postWithToken(
            "articles",
            WrappedArticleBody(createArticleBody1),
            jwt
          )
          rs2 <- postWithToken(
            "articles",
            WrappedArticleBody(createArticleBody2),
            jwt
          )
          rs3 <- postWithToken(
            "articles",
            WrappedArticleBody(createArticleBody3),
            jwt
          )
          rs4 <- get("tags")
          tags <- rs4.as[GetTagsOutput].map(_.tags)
        } yield {
          rs1.status ==> Status.Ok
          rs2.status ==> Status.Ok
          rs3.status ==> Status.Ok
          rs4.status ==> Status.Ok
          tags.size ==> (tags1 ++ tags2 ++ tags3).toSet.size
          tags.head ==> mostPopularTag
        }

        t.unsafeRunSync()
      }
    }
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
    val articleApis = ArticleApis.impl(
      articleRepo,
      followerRepo,
      tagRepo,
      favoriteRepo,
      idHasher
    )
    val tagApis = TagApis.impl(tagRepo)
    mkApp(
      List(
        UserRoutes(userApis),
        ArticleRoutes(articleApis),
        TagRoutes(tagApis)
      )
    )
  }
}
