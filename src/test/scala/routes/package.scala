package test.io.rw.app

import cats.effect.IO
import io.circe.{Encoder, Json, ParsingFailure}
import io.circe
import io.circe.generic.auto.*
import io.rw.app.data.*
import io.rw.app.data.ApiErrors.*
import io.rw.app.data.ApiInputs.*
import io.rw.app.data.ApiOutputs.*
import io.rw.app.data.RequestBodies.*
import io.rw.app.routes.*
import io.rw.app.utils.*
import io.rw.app.security.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.headers.Authorization
import org.http4s.implicits.*
import tsec.mac.jca.HMACSHA256
import org.typelevel.ci.CIString

package object routes {

  import io.rw.app.data.RequestBodies.RegisterUserBody

  val key = HMACSHA256.unsafeBuildKey("secret_key".getBytes)
  val token = JwtToken.impl(key, 60)

  def mkApp(routes: List[AppRoutes[IO]]): HttpApp[IO] = mkHttpApp(routes, token)

  def get(path: String)(implicit app: HttpApp[IO]): IO[Response[IO]] = {
    val rq = Request[IO](method = Method.GET, uri = mkApiUri(path))
    runRq(rq)
  }

  def getWithToken(path: String, jwt: String)(implicit
      app: HttpApp[IO]
  ): IO[Response[IO]] = {
    val rq = Request[IO](method = Method.GET, uri = mkApiUri(path))
    runRq(withToken(rq, jwt))
  }

  def post[B](path: String)(implicit app: HttpApp[IO]): IO[Response[IO]] = {
    val rq = Request[IO](method = Method.POST, uri = mkApiUri(path))
    runRq(rq)
  }

  def post[B](path: String, entity: B)(implicit
      app: HttpApp[IO],
      encoder: Encoder[B]
  ): IO[Response[IO]] = {
    val rq =
      Request[IO](method = Method.POST, uri = mkApiUri(path)).withEntity(entity)
    runRq(rq)
  }

  def postWithToken[B](path: String, jwt: String)(implicit
      app: HttpApp[IO]
  ): IO[Response[IO]] = {
    val rq = Request[IO](method = Method.POST, uri = mkApiUri(path))
    runRq(withToken(rq, jwt))
  }

  def postWithToken[B](path: String, entity: B, jwt: String)(implicit
      app: HttpApp[IO],
      encoder: Encoder[B]
  ): IO[Response[IO]] = {
    val rq =
      Request[IO](method = Method.POST, uri = mkApiUri(path)).withEntity(entity)
    runRq(withToken(rq, jwt))
  }

  def put[B](path: String, entity: B)(implicit
      app: HttpApp[IO],
      encoder: Encoder[B]
  ): IO[Response[IO]] = {
    val rq =
      Request[IO](method = Method.PUT, uri = mkApiUri(path)).withEntity(entity)
    runRq(rq)
  }

  def putWithToken[B](path: String, entity: B, jwt: String)(implicit
      app: HttpApp[IO],
      encoder: Encoder[B]
  ): IO[Response[IO]] = {
    val rq =
      Request[IO](method = Method.PUT, uri = mkApiUri(path)).withEntity(entity)
    runRq(withToken(rq, jwt))
  }

  def delete(path: String)(implicit app: HttpApp[IO]): IO[Response[IO]] = {
    val rq = Request[IO](method = Method.DELETE, uri = mkApiUri(path))
    runRq(rq)
  }

  def deleteWithToken(path: String, jwt: String)(implicit
      app: HttpApp[IO]
  ): IO[Response[IO]] = {
    val rq = Request[IO](method = Method.DELETE, uri = mkApiUri(path))
    runRq(withToken(rq, jwt))
  }

  def mkApiUri(path: String): Uri =
    Uri.unsafeFromString(s"api/$path")

  def runRq(rq: Request[IO])(implicit app: HttpApp[IO]): IO[Response[IO]] =
    app.run(rq)

  def withToken(rq: Request[IO], jwt: String): Request[IO] =
    rq.withHeaders(Authorization(Credentials.Token(CIString("Token"), jwt)))

  def logon(body: Json)(implicit app: HttpApp[IO]): IO[String] =
    for {
      rs <- post("users", WrappedUserBody(body))
      jwt <- rs.as[RegisterUserOutput].map(_.user.token)
    } yield jwt

  def registerUserBody(username: String, email: String, password: String): Json = circe.parser.parse(s"""
  {
    "username": "$username",
    "email": "$email",
    "password": "$password"
  }
  """) match {
    case Right(json) => json
    case Left(error) => throw new Exception(error)
  }
}
