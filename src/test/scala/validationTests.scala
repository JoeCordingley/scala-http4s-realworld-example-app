package test.io.rw.app

import cats.data._
import cats.implicits._
import io.rw.app.valiation._
import io.rw.app.valiation.validators._
import utest._

object validationTests extends TestSuite {

  val tests = Tests {
    test("test validators") {
      test("not blank") {
        test("should return value if not blank") {
          notBlank("abc") ==> "abc".validNec
          notBlank(" ") ==> " ".validNec
        }

        test("should return error if blank") {
          notBlank("") ==> "can't be blank".invalidNec
        }
      }

      test("min") {
        test("should return value if its size not less than minimum") {
          min("abc", 2) ==> "abc".validNec
          min("a", 1) ==> "a".validNec
          min("", 0) ==> "".validNec
        }

        test("should return error if value too short") {
          min("abc", 4) ==> "is too short (minimum is 4 character)".invalidNec
          min("", 1) ==> "is too short (minimum is 1 character)".invalidNec
        }
      }

      test("max") {
        test("should return value if its size not greater than maximum") {
          max("abc", 5) ==> "abc".validNec
          max("a", 1) ==> "a".validNec
          max("", 0) ==> "".validNec
        }

        test("should return error if value too long") {
          max("abc", 2) ==> "is too long (maximum is 2 character)".invalidNec
          max("1", 0) ==> "is too long (maximum is 0 character)".invalidNec
        }
      }

      test("looks like email") {
        test("should return email if it looks like email") {
          looksLikeEmail("abc@sdf.com") ==> "abc@sdf.com".validNec
          looksLikeEmail("abc1.232@123sdf.com") ==> "abc1.232@123sdf.com".validNec
          looksLikeEmail("abc1@sdf1.com.123") ==> "abc1@sdf1.com.123".validNec
        }

        test("should return error if it doesn't look like email") {
          looksLikeEmail("@sdf.com") ==> "is invalid".invalidNec
          looksLikeEmail("abc123.2123sdf.com") ==> "is invalid".invalidNec
          looksLikeEmail("abc1@sdf1com") ==> "is invalid".invalidNec
        }
      }
    }
  }
}
