package json

import org.scalacheck.{Properties, Gen, Arbitrary}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import scala.language.adhocExtensions

object JsonSchemaPropertyTests extends Properties("JsonSchema") {

//  sealed trait JsonType:
//    type T
//
     
//    type Aux[A] = JsonType{type T = A}
//    case object JsonStringT extends JsonType{type T = String}
//    case class JsonArrayT[A](t: Aux[A]) extends JsonType{type T = JsonArray[A]}
//    def gen: Gen[JsonType] = Gen.oneOf(Gen.const(JsonType.JsonStringT), Gen.lzy(gen.map(t => JsonArrayT(t))))
//    def length: JsonType => Int = {
//      case JsonStringT => 1
//      case JsonArrayT(l) => length(l) + 1
//    }
//    given Arbitrary[JsonType] = Arbitrary(gen)

  

  sealed trait UntypedJson:
    type T
    val typed: JsonType[T]

  object UntypedJson:
    def fromTyped[A](t: JsonType[A]): UntypedJson = new UntypedJson{
      type T = A
      val typed: JsonType[T] = t
    }

  object JsonType:
    def gen: Gen[UntypedJson] = Gen.oneOf(
      Gen.const(JsonType.StringType).map(UntypedJson.fromTyped), 
      Gen.lzy(gen).map(_.typed).map(ArrayType(_)).map(UntypedJson.fromTyped)
    )
    
  enum JsonType[A]:
    case StringType extends JsonType[String]
    case ArrayType[A](t: JsonType[A]) extends JsonType[JsonArray[A]]

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

//  property("not massive") = forAll { (a: JsonType) =>
//    JsonType.length(a) <= 10
//  }

}

