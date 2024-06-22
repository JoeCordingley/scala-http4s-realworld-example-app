package test.io.rw.app.routes

import json.*
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
import org.http4s.client.Client
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.syntax.literals.uri
import tsec.mac.jca.HMACSHA256
import test.io.rw.app.WithEmbededDbTestSuite
import utest.*
import cats.effect.unsafe.implicits.global
import io.circe
import io.rw.app.utils.mkHttpApp
import org.http4s.headers.Authorization
import org.typelevel.ci.CIString

object UserRoutesTests extends TestSuite {

  val tests = Tests {
    test("register") {
      test("new user should register and get valid token back") {
        val app: HttpApp[IO] = userRoutesApp {
          case UserApiInput.RegisterUserInput(
                "username",
                "email@email.com",
                "password123"
              ) =>
            EitherT.rightT(
              User(
                email = "email@email.com",
                token = "token",
                username = "username",
                bio = Some("bio"),
                image = Some("image")
              )
            )
          case x => throw new Exception(s"unexpected $x")
        }
        val request = Request[IO](method = Method.POST, uri = uri"/api/users")
          .withEntity(
            registerBody("username", "email@email.com", "password123")
          )

        val t = for {
          rs <- app.run(request)
          user <- rs.as[RegisterUserOutput].map(_.user)
        } yield {
          rs.status ==> Status.Ok
          user.username ==> "username"
          user.email ==> "email@email.com"
          user.token ==> "token"
        }

        t.unsafeRunSync()
      }

      test("new user with invalid email should get error") {
        val app: HttpApp[IO] = userRoutesApp()

        val request = Request[IO](method = Method.POST, uri = uri"/api/users")
          .withEntity(
            registerBody("username", "emailemail.com", "password123")
          )
        val t = for {
          rs <- app.run(request)
          json <- rs.as[Json]
        } yield {
          rs.status ==> Status.UnprocessableEntity
          json ==> json"""{"errors":{"body":["invalid email"]}} """
        }

        t.unsafeRunSync()
      }

      test("new user with short password should get error") {
        val request = Request[IO](method = Method.POST, uri = uri"/api/users")
          .withEntity(
            registerBody("username", "email@email.com", "passwor")
          )

        val t = for {
          rs <- userRoutesApp()
            .run(request)
          json <- rs.as[Json]
        } yield {
          rs.status ==> Status.UnprocessableEntity
          json ==> json"""{"errors":{"body":["password is invalid"]}} """
        }

        t.unsafeRunSync()
      }

      def errorsFromResponse: ErrorsListJson => List[String] =
        rs => JsonObject.getSoloValue(JsonObject.getSoloValue(rs)).elements

      test(
        "new user with empty username, invalid email and short password should get errors"
      ) {
        val request = Request[IO](method = Method.POST, uri = uri"/api/users")
          .withEntity(
            registerBody("", "emailemail.com", "passwor")
          )

        val (status, errors) = {
          for {
            response <- userRoutesApp()
              .run(request)
            value <- response.as[ErrorsListJson]
          } yield (response.status, errorsFromResponse(value))
        }.unsafeRunSync()

        assert(status == Status.UnprocessableEntity)
        assert(
          List("password is invalid", "invalid email", "invalid username")
            .forall(errors.contains)
        )
        assert(errors.size == 3)
      }

      test("new user with existing username should get error") {
        val app: HttpApp[IO] = userRoutesApp {
          case UserApiInput.RegisterUserInput(
                "username",
                "email@email.com",
                "password123"
              ) =>
            EitherT.leftT(
              ApiErrors.UsernameAlreadyExists
            )
          case x => throw new Exception(s"unexpected $x")
        }
        val request = Request[IO](method = Method.POST, uri = uri"/api/users")
          .withEntity(
            registerBody("username", "email@email.com", "password123")
          )

        val t = for {
          rs <- app.run(request)
          json <- rs.as[Json]
        } yield {
          rs.status ==> Status.UnprocessableEntity
          json ==> json"""{"errors":{"body":["username taken"]}} """
        }

        t.unsafeRunSync()

      }

      test("new user with existing email should get error") {
        val app: HttpApp[IO] = userRoutesApp {
          case UserApiInput.RegisterUserInput(
                "username",
                "email@email.com",
                "password123"
              ) =>
            EitherT.leftT(
              ApiErrors.EmailAlreadyExists
            )
          case x => throw new Exception(s"unexpected $x")
        }
        val request = Request[IO](method = Method.POST, uri = uri"/api/users")
          .withEntity(
            registerBody("username", "email@email.com", "password123")
          )

        val t = for {
          rs <- app.run(request)
          json <- rs.as[Json]
        } yield {
          rs.status ==> Status.UnprocessableEntity
          json ==> json"""{"errors":{"body":["email taken"]}} """
        }

        t.unsafeRunSync()
      }
    }

    test("authenticate") {
      test("existing user should authenticate and get valid token back") {
        val app: HttpApp[IO] = userRoutesApp {
          case UserApiInput.AuthenticateUserInput(
                "email@email.com",
                "password123"
              ) =>
            EitherT.rightT(
              User(
                email = "email@email.com",
                token = "token",
                username = "username",
                bio = Some("bio"),
                image = Some("image")
              )
            )
          case x => throw new Exception(s"unexpected $x")
        }
        val request =
          Request[IO](method = Method.POST, uri = uri"/api/users/login")
            .withEntity(
              authenticateUserBody("email@email.com", "password123")
            )

        val t = for {
          rs <- app.run(request)
          json <- rs.as[Json]
        } yield {
          rs.status ==> Status.Ok
          json ==> json"""
          {
            "user": {
              "username": "username",
              "email": "email@email.com",
              "token": "token",
              "bio": "bio",
              "image": "image"
            }
          }
          """
        }

        t.unsafeRunSync()
      }

      test("existing user with wrong password should get error") {

        val app: HttpApp[IO] = userRoutesApp {
          case UserApiInput.AuthenticateUserInput(
                "email@email.com",
                "password12345"
              ) =>
            EitherT.leftT(
              ApiErrors.UserNotFoundOrPasswordNotMatched
            )
          case x => throw new Exception(s"unexpected $x")
        }
        val request =
          Request[IO](method = Method.POST, uri = uri"/api/users/login")
            .withEntity(
              authenticateUserBody("email@email.com", "password12345")
            )

        val t = for {
          rs <- app.run(request)
          json <- rs.as[Json]
        } yield {
          rs.status ==> Status.UnprocessableEntity
          json ==> json"""{
            "errors": {"body": ["email or password is invalid"]}
          }"""
        }

        t.unsafeRunSync()
      }
    }

    test("get") {
      test("authenticated user should get itself with valid token back") {
        val apis: UserApis[IO] = {
          case UserApiInput.GetUserInput(42) =>
            EitherT.rightT(
              User(
                email = "email@email.com",
                token = "token",
                username = "username",
                bio = Some("bio"),
                image = Some("image")
              )
            )
          case x => throw new Exception(s"unexpected $x")
        }
        val token: JwtToken[IO] = new JwtToken[IO] {
          override def validate(token: String): IO[Option[JwtTokenPayload]] =
            if (token == "someToken") then IO.pure(Some(JwtTokenPayload(42)))
            else throw new Exception(s"unexpected $token")
          override def generate(payload: JwtTokenPayload): IO[String] = ???
        }
        val app = userRoutesApp(apis, token)

        val request =
          Request[IO](method = Method.GET, uri = uri"/api/user")
            .withHeaders(
              Authorization(Credentials.Token(CIString("Token"), "someToken"))
            )

        val t = for {
          rs <- app.run(request)
          json <- rs.as[Json]
        } yield {
          rs.status ==> Status.Ok
          json ==> json"""
          {
            "user": {
              "username": "username",
              "email": "email@email.com",
              "token": "token",
              "bio": "bio",
              "image": "image"
            }
          }
          """
        }

        t.unsafeRunSync()
      }

      test("authenticated user should get not found when user does not exist") {
        val apis: UserApis[IO] = {
          case UserApiInput.GetUserInput(42) =>
            EitherT.leftT(
              ApiErrors.UserNotFound
            )
          case x => throw new Exception(s"unexpected $x")
        }
        val token: JwtToken[IO] = new JwtToken[IO] {
          override def validate(token: String): IO[Option[JwtTokenPayload]] =
            if (token == "someToken") then IO.pure(Some(JwtTokenPayload(42)))
            else throw new Exception(s"unexpected $token")
          override def generate(payload: JwtTokenPayload): IO[String] = ???
        }
        val app = userRoutesApp(apis, token)

        val request =
          Request[IO](method = Method.GET, uri = uri"/api/user")
            .withHeaders(
              Authorization(Credentials.Token(CIString("Token"), "someToken"))
            )

        val t = for {
          rs <- app.run(request)
        } yield {
          rs.status ==> Status.NotFound
        }

        t.unsafeRunSync()
      }

      test("not authenticated user should get error") {
        val app = userRoutesApp()
        val request =
          Request[IO](method = Method.GET, uri = uri"/api/user")
        val t = for {
          rs <- app.run(request)
        } yield {
          rs.status ==> Status.Unauthorized
        }

        t.unsafeRunSync()
      }

      test("user with invalid token should get error") {
        val token: JwtToken[IO] = new JwtToken[IO] {
          override def validate(token: String): IO[Option[JwtTokenPayload]] =
            if (token == "someToken") then IO.pure(None)
            else throw new Exception(s"unexpected $token")
          override def generate(payload: JwtTokenPayload): IO[String] = ???
        }
        val request =
          Request[IO](method = Method.GET, uri = uri"/api/user")
            .withHeaders(
              Authorization(Credentials.Token(CIString("Token"), "someToken"))
            )
        val app = userRoutesApp(token = token)

        val t = for {
          rs <- app.run(request)
        } yield {
          rs.status ==> Status.Unauthorized
        }

        t.unsafeRunSync()
      }
    }

    test("update") {
      test("authenticated user should update itself and get valid token back") {
        val token: JwtToken[IO] = new JwtToken[IO] {
          override def validate(token: String): IO[Option[JwtTokenPayload]] =
            if (token == "someToken") then IO.pure(Some(JwtTokenPayload(42)))
            else throw new Exception(s"unexpected $token")
          override def generate(payload: JwtTokenPayload): IO[String] = ???
        }
        val apis: UserApis[IO] = {
          case i
              if i == UserApiInput.UpdateUserInput(
                authUser = 42,
                username = Some("username1"),
                email = None,
                password = None,
                bio = None,
                image = Some("image")
              ) =>
            EitherT.rightT(
              User(
                email = "email@email.com",
                token = "token",
                username = "username",
                bio = Some("bio"),
                image = Some("image")
              )
            )
          case x => throw new Exception(s"unexpected $x")
        }
        val request = Request[IO](method = Method.PUT, uri = uri"/api/user")
          .withEntity(
            json"""{
              "user": {
                "username": "username1",
                "image": "image"
              }
            }"""
          )
          .withHeaders(
            Authorization(Credentials.Token(CIString("Token"), "someToken"))
          )

        val t = for {
          rs <- userRoutesApp(apis, token).run(request)
          json <- rs.as[Json]
        } yield {
          rs.status ==> Status.Ok
          json ==> json""" {
            "user": {
              "username": "username",
              "email": "email@email.com",
              "token": "token",
              "bio": "bio",
              "image": "image"
            }
          }
          """
        }

        t.unsafeRunSync()
      }
    }
    test("not authenticated user should get error") {
      val app = userRoutesApp()
      val request =
        Request[IO](method = Method.PUT, uri = uri"/api/user")
          .withEntity(json"""{
            "user": {
              "username": "username1"
            }
          }""")
      val t = for {
        rs <- app.run(request)
      } yield {
        rs.status ==> Status.Unauthorized
      }

      t.unsafeRunSync()
    }
    test("user with invalid token should get error") {
      val token: JwtToken[IO] = new JwtToken[IO] {
        override def validate(token: String): IO[Option[JwtTokenPayload]] =
          if (token == "someToken") then IO.pure(None)
          else throw new Exception(s"unexpected $token")
        override def generate(payload: JwtTokenPayload): IO[String] = ???
      }
      val request =
        Request[IO](method = Method.PUT, uri = uri"/api/user")
          .withEntity(json"""{
            "user": {
              "username": "username1"
            }
          }""")
          .withHeaders(
            Authorization(Credentials.Token(CIString("Token"), "someToken"))
          )
      val app = userRoutesApp(token = token)

      val t = for {
        rs <- app.run(request)
      } yield {
        rs.status ==> Status.Unauthorized
      }

      t.unsafeRunSync()
    }

  }

  def userRoutesApp(
      userApis: UserApis[IO] = _ => throw new Exception("not implemented"),
      token: JwtToken[IO] = someToken[IO]
  ): HttpApp[IO] = mkHttpApp(
    List(UserRoutes(userApis)),
    token
  )

  def someToken[F[_]]: JwtToken[F] = new JwtToken[F] {
    override def generate(payload: JwtTokenPayload): F[String] = ???
    override def validate(token: String): F[Option[JwtTokenPayload]] = ???
  }

  def registerBody(username: String, email: String, password: String): Json =
    circe.parser
      .parse(s"""{
            "user": {
              "username": "$username",
              "email": "$email",
              "password": "$password"
            }
          }""")
      .toOption
      .get
  def authenticateUserBody(
      email: String,
      password: String
  ): Json =
    circe.parser
      .parse(s"""{
            "user": {
              "email": "$email",
              "password": "$password"
            }
          }""")
      .toOption
      .get
}
