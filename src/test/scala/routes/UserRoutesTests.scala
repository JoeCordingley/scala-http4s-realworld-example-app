package test.io.rw.app.routes

import cats.*
import cats.data.*
import cats.effect.IO
import io.circe.generic.auto.*
import io.circe.Json
import io.circe.literal.*
import io.rw.app.apis.*
import io.rw.app.data.*
import io.rw.app.data.ApiErrors.*
import io.rw.app.data.ApiInputs.*
import io.rw.app.data.ApiOutputs.*
import io.rw.app.data.RequestBodies.*
import io.rw.app.repos.*
import io.rw.app.routes.*
import io.rw.app.security.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import tsec.mac.jca.HMACSHA256
import test.io.rw.app.WithEmbededDbTestSuite
import utest.*
import cats.effect.unsafe.implicits.global

object UserRoutesTests extends WithEmbededDbTestSuite {

  val tests = Tests {
    test("register") {
      test("new user should register and get valid token back") {
        val registerBody =
          RegisterUserBody("username", "email@email.com", "password123")

        val t = for {
          rs <- post("users", WrappedUserBody(registerBody))
          user <- rs.as[RegisterUserOutput].map(_.user)
          validToken <- token.validate(user.token)
        } yield {
          rs.status ==> Status.Ok
          user.username ==> registerBody.username
          user.email ==> registerBody.email
          validToken.isDefined ==> true
        }

        t.unsafeRunSync()
      }

      test("new user with invalid email should get error") {
        val registerBody =
          RegisterUserBody("username", "emailemail.com", "password123")

        val t = for {
          rs <- post("users", WrappedUserBody(registerBody))
          errors <- rs.as[ValidationErrorResponse].map(_.errors)
        } yield {
          rs.status ==> Status.UnprocessableEntity
          errors.size ==> 1
          errors.get("email") ==> Some(List("is invalid"))
        }

        t.unsafeRunSync()
      }

      test("new user with short password shold get error") {
        val registerBody =
          RegisterUserBody("username", "email@email.com", "passwor")

        val t = for {
          rs <- post("users", WrappedUserBody(registerBody))
          errors <- rs.as[ValidationErrorResponse].map(_.errors)
        } yield {
          rs.status ==> Status.UnprocessableEntity
          errors.size ==> 1
          errors.get("password") ==> Some(
            List("is too short (minimum is 8 character)")
          )
        }

        t.unsafeRunSync()
      }

      test(
        "new user with empty username, invalid email and short password shold get errors"
      ) {
        val registerBody = RegisterUserBody("", "emailemail.com", "passwor")

        val t = for {
          rs <- post("users", WrappedUserBody(registerBody))
          errors <- rs.as[ValidationErrorResponse].map(_.errors)
        } yield {
          rs.status ==> Status.UnprocessableEntity
          errors.size ==> 3
          errors.get("username") ==> Some(
            List("can't be blank", "is too short (minimum is 1 character)")
          )
          errors.get("password") ==> Some(
            List("is too short (minimum is 8 character)")
          )
          errors.get("email") ==> Some(List("is invalid"))
        }

        t.unsafeRunSync()
      }

      test("new user with existing username should get error") {
        val registerBody1 =
          RegisterUserBody("username", "email@email.com", "password123")
        val registerBody2 =
          RegisterUserBody("username", "email_1@email.com", "password123")

        val t = for {
          rs1 <- post("users", WrappedUserBody(registerBody1))
          rs2 <- post("users", WrappedUserBody(registerBody2))
          errors <- rs2.as[ValidationErrorResponse].map(_.errors)
        } yield {
          rs1.status ==> Status.Ok
          rs2.status ==> Status.UnprocessableEntity
          errors.size ==> 1
          errors.get("username") ==> Some(List("has already been taken"))
        }

        t.unsafeRunSync()
      }

      test("new user with existing email should get error") {
        val registerBody1 =
          RegisterUserBody("username", "email@email.com", "password123")
        val registerBody2 =
          RegisterUserBody("username_1", "email@email.com", "password123")

        val t = for {
          rs1 <- post("users", WrappedUserBody(registerBody1))
          rs2 <- post("users", WrappedUserBody(registerBody2))
          errors <- rs2.as[ValidationErrorResponse].map(_.errors)
        } yield {
          rs1.status ==> Status.Ok
          rs2.status ==> Status.UnprocessableEntity
          errors.size ==> 1
          errors.get("email") ==> Some(List("has already been taken"))
        }

        t.unsafeRunSync()
      }
    }

    test("authenticate") {
      test("existing user should authenticate and get valid token back") {
        val registerBody =
          RegisterUserBody("username", "email@email.com", "password123")
        val authenticateBody = json"""
          {
            "email": "email@email.com",
            "password": "password123"
          }
        """

        val t = for {
          rs1 <- post("users", WrappedUserBody(registerBody))
          rs2 <- post("users/login", WrappedUserBody(authenticateBody))
          user <- rs2.as[AuthenticateUserOutput].map(_.user)
          payload <- token.validate(user.token)
        } yield {
          rs2.status ==> Status.Ok
          user.email ==> "email@email.com"
          payload.isDefined ==> true
        }

        t.unsafeRunSync()
      }

      test("existing user with wrong password should get error") {
        val registerBody =
          RegisterUserBody("username", "email@email.com", "password123")
        val authenticateBody = json"""
          {
            "email": "email@email.com",
            "password": "password12345"
          }
        """

        val t = for {
          rs1 <- post("users", WrappedUserBody(registerBody))
          rs2 <- post("users/login", WrappedUserBody(authenticateBody))
          errors <- rs2.as[ValidationErrorResponse].map(_.errors)
        } yield {
          rs2.status ==> Status.UnprocessableEntity
          errors.size ==> 1
          errors.get("email or password") ==> Some(List("is invalid"))
        }

        t.unsafeRunSync()
      }

      test("non existing user should get error") {
        val registerBody = json"""
          {
            "email": "email@email.com",
            "password": "password123"
          }
        """

        val t = for {
          rs <- post("users/login", WrappedUserBody(registerBody))
          errors <- rs.as[ValidationErrorResponse].map(_.errors)
        } yield {
          rs.status ==> Status.UnprocessableEntity
          errors.size ==> 1
          errors.get("email or password") ==> Some(List("is invalid"))
        }

        t.unsafeRunSync()
      }
    }

    test("get") {
      test("authenticated user should get itself with valid token back") {
        val registerBody =
          RegisterUserBody("username", "email@email.com", "password123")

        val t = for {
          rs1 <- post("users", WrappedUserBody(registerBody))
          jwt <- rs1.as[RegisterUserOutput].map(_.user.token)
          rs2 <- getWithToken("user", jwt)
          user <- rs2.as[GetUserOutput].map(_.user)
          payload <- token.validate(user.token)
        } yield {
          rs2.status ==> Status.Ok
          user.username ==> registerBody.username
          user.email ==> registerBody.email
          payload.isDefined ==> true
        }

        t.unsafeRunSync()
      }

      test("authenticated user should get not found when user does not exist") {
        val t = for {
          jwt <- token.generate(JwtTokenPayload(1))
          rs <- getWithToken("user", jwt)
        } yield {
          rs.status ==> Status.NotFound
        }

        t.unsafeRunSync()
      }

      test("not authenticated user should get error") {
        val t = for {
          rs <- get("user")
        } yield {
          rs.status ==> Status.Unauthorized
        }

        t.unsafeRunSync()
      }

      test("user with invalid token should get error") {
        val anotherKey =
          HMACSHA256.unsafeBuildKey("secret_key_for_another_token_123".getBytes)
        val anotherToken = JwtToken.impl(anotherKey, 60)

        val t = for {
          anotherJwt <- anotherToken.generate(JwtTokenPayload(1))
          rs <- getWithToken("user", anotherJwt)
        } yield {
          rs.status ==> Status.Unauthorized
        }

        t.unsafeRunSync()
      }
    }

    test("update") {
      test("authenticated user should update itself and get valid token back") {
        val registerBody =
          RegisterUserBody("username", "email@email.com", "password123")
        val updateBody =
          UpdateUserBody(Some("username1"), None, None, None, Some("image"))

        val t = for {
          rs1 <- post("users", WrappedUserBody(registerBody))
          jwt <- rs1.as[RegisterUserOutput].map(_.user.token)
          rs2 <- putWithToken("user", WrappedUserBody(updateBody), jwt)
          user <- rs2.as[UpdateUserOutput].map(_.user)
          payload <- token.validate(user.token)
        } yield {
          rs2.status ==> Status.Ok
          Some(user.username) ==> updateBody.username
          user.image ==> updateBody.image
          user.email ==> registerBody.email
          payload.isDefined ==> true
        }

        t.unsafeRunSync()
      }

      test("not authenticated user should get error") {
        val registerBody =
          UpdateUserBody(Some("username1"), None, None, None, None)

        val t = for {
          rs <- put("user", WrappedUserBody(registerBody))
        } yield {
          rs.status ==> Status.Unauthorized
        }

        t.unsafeRunSync()
      }

      test("user with invalid token should get error") {
        val anotherKey =
          HMACSHA256.unsafeBuildKey("secret_key_for_another_token_123".getBytes)
        val anotherToken = JwtToken.impl(anotherKey, 60)
        val registerBody =
          UpdateUserBody(Some("username1"), None, None, None, None)

        val t = for {
          anotherJwt <- anotherToken.generate(JwtTokenPayload(1))
          rs <- putWithToken("user", WrappedUserBody(registerBody), anotherJwt)
        } yield {
          rs.status ==> Status.Unauthorized
        }

        t.unsafeRunSync()
      }
    }
  }
  implicit val app: HttpApp[IO] = {
    val passwordHasher = PasswordHasher.impl
    val userRepo = UserRepo.impl(xa)
    val apis = UserApis.impl(passwordHasher, token, userRepo)
    mkApp(List(UserRoutes(apis)))
  }
}
