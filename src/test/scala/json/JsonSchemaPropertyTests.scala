package json

import org.scalacheck.{Properties, Gen, Arbitrary}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import scala.language.adhocExtensions

object JsonSchemaPropertyTests extends Properties("JsonSchema") {
  sealed trait DependentlyTyped:
    type T
    val value: T

  object DependentlyTyped:
    def enclose[A](a: A): DependentlyTyped = new DependentlyTyped{
      type T = A
      val value: T = a
    }


  sealed trait UntypedJson:
    type T
    val typed: JsonType[T]

  object JsonType:
    def gen: Gen[UntypedJson] = Gen.oneOf(
      Gen.const(StringType),
      Gen.lzy(gen).map(_.typed).map(ArrayType(_))
    )
    case object StringType extends JsonType[String]
    case class ArrayType[A](t: JsonType[A]) extends JsonType[JsonArray[A]]

  sealed trait JsonType[A] extends UntypedJson{
    type T = A
    val typed: JsonType[T] = this
  }


  def genJson[A](jsonType: JsonType[A]): Gen[A] = jsonType match {
    case JsonType.StringType => arbitrary[String]
    case JsonType.ArrayType(a) => Gen.listOf(Gen.lzy(genJson(a))).map(JsonArray(_))
  }

  property("startsWith") = forAll { (a: String, b: String) =>
    (a+b).startsWith(a)
  }

  property("concatenate") = forAll { (a: String, b: String) =>
    (a+b).length > a.length && (a+b).length > b.length
  }

  property("substring") = forAll { (a: String, b: String, c: String) =>
    (a+b+c).substring(a.length, a.length+b.length) == b
  }


}

