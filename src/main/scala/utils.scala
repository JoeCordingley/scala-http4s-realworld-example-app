package io.rw.app

import cats.data.Kleisli
import cats.effect.IO
import cats.implicits.*
import java.text.Normalizer
import java.text.Normalizer.Form
import io.rw.app.data.*
import io.rw.app.routes.*
import io.rw.app.security.*
import org.hashids.Hashids
import org.http4s.*
import org.http4s.implicits.*
import org.http4s.server.*
import org.http4s.server.middleware.CORS

object utils {

  trait IdHasher {
    def hash(id: Long): String
  }

  object IdHasher {
    def impl(salt: String) = new IdHasher {
      val hashids = new Hashids(salt, 5)

      def hash(id: Long): String =
        hashids.encode(id)
    }
  }

  val notAsciiRe = "[^\\p{ASCII}]".r
  val notWordsRs = "[^\\w]".r
  val spacesRe = "\\s+".r
  def slugify(s: String): String = {
    // get rid of fancy characters accents
    val normalized = Normalizer.normalize(s, Form.NFD)
    val cleaned = notAsciiRe.replaceAllIn(normalized, "")

    val wordsOnly = notWordsRs.replaceAllIn(cleaned, " ").trim
    spacesRe.replaceAllIn(wordsOnly, "-").toLowerCase
  }

  val tokenPattern = "^Token (.+)".r
  def extractTokenValue(s: String): Option[String] =
    s match {
      case tokenPattern(token) => Some(token)
      case _                   => None
    }

  def mkHttpApp(
      appRoutes: List[AppRoutes[IO]],
      token: JwtToken[IO]
  ): HttpApp[IO] = {
    val authMiddleware = AuthMiddleware(authUser[IO](token))
    val routes = authMiddleware(appRoutes.reduce(_ <+> _))

    Router("/api" -> CORS.policy(routes)).orNotFound
  }
}
