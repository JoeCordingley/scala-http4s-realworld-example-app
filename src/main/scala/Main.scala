package io.rw.app

import cats.*
import cats.data.*
import cats.effect.*
import cats.implicits.*
import doobie.util.transactor.Transactor
import io.rw.app.apis.*
import io.rw.app.data.*
import io.rw.app.repos.*
import io.rw.app.routes.*
import io.rw.app.security.*
import io.rw.app.utils.*
import org.http4s.*
import org.http4s.implicits.*
import org.http4s.server.*
import org.http4s.blaze.server.BlazeServerBuilder
import pureconfig.*
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
    val xa = Transactor.fromDriverManager[IO](
      "org.postgresql.Driver",
      config.dbUrl,
      config.dbUser,
      config.dbPassword
    )
    val userRepo = UserRepo.impl(xa)
    val followerRepo = FollowerRepo.impl(xa)
    val tagRepo = TagRepo.impl(xa)
    val articleRepo = ArticleRepo.impl(xa)
    val favoriteRepo = FavoriteRepo.impl(xa)
    val commentRepo = CommentRepo.impl(xa)

    val userApis = UserApis.impl(passwordHasher, token, userRepo)
    val profileApis = ProfileApis.impl(userRepo, followerRepo)
    val articleApis = ArticleApis.impl(
      articleRepo,
      followerRepo,
      tagRepo,
      favoriteRepo,
      idHasher
    )
    val tagApis = TagApis.impl(tagRepo)
    val commentApis = CommentApis.impl(commentRepo, articleRepo, followerRepo)

    val routes: List[AppRoutes[IO]] = List(
      UserRoutes(userApis),
      ProfileRoutes(profileApis),
      ArticleRoutes(articleApis),
      CommentRoutes(commentApis),
      TagRoutes(tagApis)
    )
    val httpApp = mkHttpApp(routes, token)

    BlazeServerBuilder[IO]
      .bindHttp(config.apiPort, config.apiHost)
      .withHttpApp(httpApp)
      .serve
      .compile
      .drain
      .as(ExitCode.Success)
  }
}
