package test.io.rw.app

import io.rw.app.utils.*
import utest.*

object utilsTests extends TestSuite {

  val tests = Tests {
    test("test slugify") {
      test("should trim spaces") {
        slugify("  abc   ") ==> "abc"
        slugify(" abc") ==> "abc"
        slugify("abc ") ==> "abc"
        slugify(" abc ") ==> "abc"
      }

      test("should replaces spaces between words with dash") {
        slugify("abc def") ==> "abc-def"
        slugify("abc  def") ==> "abc-def"
        slugify("abc  def ghi") ==> "abc-def-ghi"
        slugify("abc  def    ghi") ==> "abc-def-ghi"
      }

      test("should remove non word characters") {
        slugify("abc 123 ...") ==> "abc-123"
        slugify("abc 123 .,! e123g") ==> "abc-123-e123g"
        slugify("abc\n @*& 123 .,! e123g") ==> "abc-123-e123g"
        slugify(
          "!@#$ abc\n \t %^&*() _ 123 .,! e123g -= _+"
        ) ==> "abc-_-123-e123g-_"
      }
    }

    test("test extract token value") {
      test("should extract token value") {
        extractTokenValue("Token 123abx90") ==> Some("123abx90")
        extractTokenValue("Token  ") ==> Some(" ")
        extractTokenValue("Token  123 456 8") ==> Some(" 123 456 8")
      }

      test("should return none where token not present") {
        extractTokenValue("Token ") ==> None
        extractTokenValue("Token") ==> None
        extractTokenValue("Tken") ==> None
        extractTokenValue("") ==> None
      }
    }
  }
}
