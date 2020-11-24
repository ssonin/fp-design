package lectures.week1

import scala.util.Try

object FunctionsAndPM extends App {

  val data = JObj(Map(
    "firstName" -> JStr("John"),
    "lastName" -> JStr("Smith"),
    "address" -> JObj(Map(
      "streetAddress" -> JStr("21 2nd Street "),
      "state" -> JStr("NY"),
      "postalCode" -> JNum(10021)
    )),
    "phoneNumbers" -> JSeq(List(
      JObj(Map(
        "type" -> JStr("home"), "number" -> JStr("212 555-1234"),
      )),
      JObj(Map(
        "type" -> JStr("fax"), "number" -> JStr("646 555-4567")
      ))
    ))
  ))

  def show(json: JSON): String = json match {
    case JSeq(elems) => "[" + elems.map(show).mkString(", ") + "]"
    case JObj(bindings) =>
      val assocs = bindings.map {
        case (k, v) => "\"" + k + "\": " + show(v)
      }
      "{" + assocs.mkString(", ") + "}"
    case JNum(num) => num.toString
    case JStr(str) => "\"" + str + "\""
    case JBool(b) => b.toString
    case JNull => null
  }

  println(show(data))

  val g: PartialFunction[List[Int], String] = {
    case Nil => "one"
    case x :: rest => {
      rest match {
        case Nil => "two"
      }
    }
  }

  // isDefinedAt guarantee applies only to the outermost pattern block
  println(g.isDefinedAt(List(1, 2, 3)))
  println(Try(g(List(1, 2, 3))))

  val json: List[JSON] = List(data)
  val wanted = for {
    JObj(bindings) <- json
    JSeq(phones) = bindings("phoneNumbers")
    JObj(phone) <- phones
    JStr(digits) = phone("number")
    if digits startsWith "212"
  } yield bindings("firstName")

  println(wanted)
  println(wanted.map(show))
}

trait JSON
case class JSeq(elems: List[JSON]) extends JSON
case class JObj(bindings: Map[String, JSON]) extends JSON
case class JNum(num: Double) extends JSON
case class JStr(str: String) extends JSON
case class JBool(b: Boolean) extends JSON
case object JNull extends JSON
