package test.io.rw.app

import cats.effect.IO
import io.rw.app.data.JwtTokenPayload
import io.rw.app.security.*
import tsec.mac.jca.HMACSHA256
import utest.*
import cats.effect.unsafe.implicits.global

object securityTests extends TestSuite {

  val tests = Tests {
    test("test hashed password") {
      val passwordHasher = PasswordHasher.impl
      val psw = "my_password_123"

      test("is valid") {
        val t = for {
          hash <- passwordHasher.hash(psw)
          valid <- passwordHasher.validate(psw, hash)
        } yield {
          valid ==> true
        }

        t.unsafeRunSync()
      }

      test("is not valid") {
        test("bad password") {
          val t = for {
            hash <- passwordHasher.hash(psw)
            valid <- passwordHasher.validate(tamper(psw), hash)
          } yield {
            valid ==> false
          }

          t.unsafeRunSync()
        }

        test("bad hash") {
          val t = for {
            hash <- passwordHasher.hash(psw)
            valid <- passwordHasher.validate(psw, tamper(hash))
          } yield {
            valid ==> false
          }

          t.unsafeRunSync()
        }
      }
    }

    test("test jwt token") {
      val payload = JwtTokenPayload(99)
      val key = HMACSHA256.buildKey[IO]("secret_key".getBytes).unsafeRunSync()
      val token = JwtToken.impl(key, 10)

      test("is valid") {
        val t = for {
          jwtStr <- token.generate(payload)
          payloadFromToken <- token.validate(jwtStr)
        } yield {
          payloadFromToken ==> Some(payload)
        }

        t.unsafeRunSync()
      }

      test("is not valid") {
        val t = for {
          jwtStr <- token.generate(payload)
          payloadFromToken <- token.validate(tamper(jwtStr))
        } yield {
          payloadFromToken ==> None
        }

        t.unsafeRunSync()
      }
    }
  }

  def tamper(s: String): String = {
    val halfLength = s.size / 2
    if (halfLength > 0)
      s.substring(0, halfLength) + ((s
        .charAt(halfLength)
        .toInt + 1) % 256).toChar + s.substring(halfLength + 1)
    else s
  }
}
