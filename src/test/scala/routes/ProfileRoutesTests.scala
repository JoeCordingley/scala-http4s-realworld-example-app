package test.io.rw.app.routes

import cats._
import cats.data._
import cats.effect.IO
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
import org.http4s._
import org.http4s.circe.CirceEntityCodec._
import test.io.rw.app.WithEmbededDbTestSuite
import utest._

object ProfileRoutesTests extends WithEmbededDbTestSuite {

  val tests = Tests {
    test("profile") {
      test("authenticated user should get profile") {
        val registerBody = RegisterUserBody("username", "email@email.com", "password123")

        val t = for {
          jwt <- logon(registerBody)
          rs <- getWithToken(s"profiles/${registerBody.username}", jwt)
          profile <- rs.as[GetProfileOutput].map(_.profile)
        } yield {
          rs.status ==> Status.Ok
          profile.username ==> registerBody.username
        }

        t.unsafeRunSync()
      }

      test("authenticated user should get not found error when profile does not exist") {
        val registerBody = RegisterUserBody("username", "email@email.com", "password123")

        val t = for {
          jwt <- logon(registerBody)
          rs <- getWithToken("profiles/username1", jwt)
        } yield {
          rs.status ==> Status.NotFound
        }

        t.unsafeRunSync()
      }

      test("not authenticated user should get profile") {
        val registerBody = RegisterUserBody("username", "email@email.com", "password123")

        val t = for {
          _ <- logon(registerBody)
          rs <- get(s"profiles/${registerBody.username}")
          profile <- rs.as[GetProfileOutput].map(_.profile)
        } yield {
          rs.status ==> Status.Ok
          profile.username ==> registerBody.username
        }

        t.unsafeRunSync()
      }
    }

    test("follow") {
      test("authenticated user should follow existing user") {
        val registerBody1 = RegisterUserBody("username1", "email1@email.com", "password123")
        val registerBody2 = RegisterUserBody("username2", "email2@email.com", "password123")

        val t = for {
          jwt <- logon(registerBody1)
          _ <- logon(registerBody2)
          rs <- postWithToken(s"profiles/${registerBody2.username}/follow", jwt)
          profile <- rs.as[FollowUserOutput].map(_.profile)
        } yield {
          rs.status ==> Status.Ok
          profile.username ==> registerBody2.username
          profile.following ==> true
        }

        t.unsafeRunSync()
      }

      test("authenticated user should get not found when profile does not exist") {
        val registerBody = RegisterUserBody("username", "email@email.com", "password123")

        val t = for {
          jwt <- logon(registerBody)
          rs <- postWithToken("profiles/username1/follow", jwt)
        } yield {
          rs.status ==> Status.NotFound
        }

        t.unsafeRunSync()
      }

      test("not authenticated user should get error") {
        val t = for {
          rs <- delete("profiles/username/follow")
        } yield {
          rs.status ==> Status.Unauthorized
        }

        t.unsafeRunSync()
      }
    }

    test("unfollow") {
      test("authenticated user should unfollow existing user") {
        val registerBody1 = RegisterUserBody("username1", "email1@email.com", "password123")
        val registerBody2 = RegisterUserBody("username2", "email2@email.com", "password123")

        val t = for {
          jwt <- logon(registerBody1)
          _ <- logon(registerBody2)
          rs1 <- postWithToken(s"profiles/${registerBody2.username}/follow", jwt)
          rs2 <- deleteWithToken(s"profiles/${registerBody2.username}/follow", jwt)
          profile <- rs2.as[FollowUserOutput].map(_.profile)
        } yield {
          rs1.status ==> Status.Ok
          rs2.status ==> Status.Ok
          profile.username ==> registerBody2.username
          profile.following ==> false
        }

        t.unsafeRunSync()
      }

      test("authenticated user should get not found when profile does not exist") {
        val registerBody = RegisterUserBody("username", "email@email.com", "password123")

        val t = for {
          jwt <- logon(registerBody)
          rs <- deleteWithToken("profiles/username1/follow", jwt)
        } yield {
          rs.status ==> Status.NotFound
        }

        t.unsafeRunSync()
      }

      test("not authenticated user should get error") {
        val t = for {
          rs <- delete("profiles/username/follow")
        } yield {
          rs.status ==> Status.Unauthorized
        }

        t.unsafeRunSync()
      }
    }

    implicit val app: HttpApp[IO] = {
      val passwordHasher = PasswordHasher.impl
      val userRepo = UserRepo.impl(xa)
      val followerRepo = FollowerRepo.impl(xa)
      val userApis = UserApis.impl(passwordHasher, token, userRepo)
      val profileApis = ProfileApis.impl(userRepo, followerRepo)
      mkApp(List(UserRoutes(userApis), ProfileRoutes(profileApis)))
    }
  }
}
