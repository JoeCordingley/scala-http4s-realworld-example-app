package io.rw.app

import cats.effect.IO
import io.circe.*
import io.circe.generic.auto.*
import io.circe.syntax.*
import io.rw.app.data.JwtTokenPayload
import tsec.jws.mac.JWTMac
import tsec.jwt.JWTClaims
import tsec.mac.jca.{HMACSHA256, MacSigningKey}
import tsec.passwordhashers.jca.SCrypt
import tsec.passwordhashers.PasswordHash
import io.rw.app.data.Password

object security {

  case class HashedPassword(value: String)

  trait PasswordHasher[F[_]] {
    def hash(psw: Password): F[HashedPassword]
    def validate(psw: Password, hash: HashedPassword): F[Boolean]
  }

  object PasswordHasher {
    def impl = new PasswordHasher[IO] {
      def hash(psw: Password): IO[HashedPassword] = SCrypt.hashpw[IO](psw.value).map(HashedPassword(_))
      def validate(psw: Password, hash: HashedPassword): IO[Boolean] =
        SCrypt.checkpwBool[IO](psw.value, PasswordHash(hash.value))
    }
  }

  trait JwtToken[F[_]] {
    def generate(payload: JwtTokenPayload): F[String]
    def validate(token: String): F[Option[JwtTokenPayload]]
  }

  object JwtToken {
    def impl(key: MacSigningKey[HMACSHA256], durationMinutes: Int) =
      new JwtToken[IO] {
        import scala.concurrent.duration.*

        val payloadKeyName = "payload"

        def generate(payload: JwtTokenPayload): IO[String] = {
          for {
            claims <- JWTClaims.withDuration[IO](
              expiration = Some(durationMinutes.minutes),
              customFields = List(payloadKeyName -> payload.asJson)
            )
            jwtStr <- JWTMac.buildToString[IO, HMACSHA256](claims, key)
          } yield jwtStr
        }

        def validate(token: String): IO[Option[JwtTokenPayload]] = {
          val payload = for {
            jwt <- JWTMac.verifyAndParse[IO, HMACSHA256](token, key)
            payloadE <- IO.pure(
              jwt.body.getCustom[JwtTokenPayload](payloadKeyName)
            )
          } yield payloadE.toOption

          // suppress error from verification
          payload.handleErrorWith(_ => IO.pure(None))
        }
      }
  }
}
