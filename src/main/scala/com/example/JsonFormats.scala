package com.example

import java.time.LocalDateTime

import com.example.Models._
import spray.json.{JsString, JsValue, JsonFormat, RootJsonFormat, deserializationError}

import scala.util.{Failure, Success, Try}

//#json-formats
import spray.json.DefaultJsonProtocol

object JsonFormats  {
  // import the default encoders for primitive types (Int, String, Lists etc)
  import DefaultJsonProtocol._

  implicit val jodaLocalDateTimeFormat: JsonFormat[LocalDateTime] =
    new JsonFormat[LocalDateTime] {
      override def write(obj: LocalDateTime): JsValue = JsString(obj.toString)

      override def read(json: JsValue): LocalDateTime = json match {
        case JsString(s) => Try(LocalDateTime.parse(s)) match {
          case Success(result) => result
          case Failure(exception) =>
            deserializationError(s"could not parse $s as Joda LocalDateTime", exception)
        }
        case notAJsString =>
          deserializationError(s"expected a String but got a $notAJsString")
      }
    }

  //implicit val optTimeJsonFormat: RootJsonFormat[Option[LocalDateTime]] = jsonFormat1(Option[LocalDateTime])
  implicit val voteJsonFormat: RootJsonFormat[Vote] = jsonFormat2(Vote)
  implicit val answerDtoJsonFormat: RootJsonFormat[AnswerDto] = jsonFormat2(AnswerDto)
  implicit val pollJsonFormat: RootJsonFormat[Poll] = jsonFormat6(Poll.apply)
  implicit val pollDtoJsonFormat: RootJsonFormat[PollDto] = jsonFormat3(PollDto)
  implicit val userDtoJsonFormat: RootJsonFormat[UserDto] = jsonFormat2(UserDto)
  implicit val answerJsonFormat: RootJsonFormat[Answer] = jsonFormat4(Answer.apply)
}
//#json-formats
