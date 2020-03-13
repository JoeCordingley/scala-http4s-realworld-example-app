package io.rw.app

import cats._
import cats.data._
import cats.effect._
import cats.implicits._
import doobie.util.transactor.Transactor
import io.rw.app.apis._
import io.rw.app.data._
import io.rw.app.repos._
import io.rw.app.routes._
import io.rw.app.security._
import io.rw.app.utils._
import org.http4s._
import org.http4s.implicits._
import org.http4s.server._
import org.http4s.server.blaze._
import pureconfig._
import pureconfig.generic.auto._
import tsec.mac.jca.HMACSHA256


object Main extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {
    // app shouldn't event start if no config is there
    val config = ConfigSource.default.loadOrThrow[AppConfig]

    val passwordHasher = PasswordHasher.impl
    val idHasher = IdHasher.impl(config.idHasherSalt)

    // TODO use safe
    val key = HMACSHA256.unsafeBuildKey(config.jwtTokenKey.getBytes)
    val token = JwtToken.impl(key, config.jwtTokenExpiration)

    // TODO use hikaricp
    val xa = Transactor.fromDriverManager[IO]("org.postgresql.Driver", config.dbUrl, config.dbUser, config.dbPassword)
    val userRepo = UserRepo.impl(xa)
    val followerRepo = FollowerRepo.impl(xa)
    val tagRepo = TagRepo.impl(xa)
    val articleRepo = ArticleRepo.impl(xa)
    val favoriteRepo = FavoriteRepo.impl(xa)
    val commentRepo = CommentRepo.impl(xa)

    val userApis = UserApis.impl(passwordHasher, token, userRepo)
    val profileApis = ProfileApis.impl(userRepo, followerRepo)
    val articleApis = ArticleApis.impl(articleRepo, followerRepo, tagRepo, favoriteRepo, idHasher)
    val tagApis = TagApis.impl(tagRepo)
    val commentApis = CommentApis.impl(commentRepo, articleRepo, followerRepo)

    val routes = List(UserRoutes(userApis), ProfileRoutes(profileApis), ArticleRoutes(articleApis), CommentRoutes(commentApis), TagRoutes(tagApis))
    val httpApp = mkHttpApp(routes, token)

    BlazeServerBuilder[IO].bindHttp(config.apiPort, config.apiHost).withHttpApp(httpApp).serve.compile.drain.as(ExitCode.Success)
  }
}
