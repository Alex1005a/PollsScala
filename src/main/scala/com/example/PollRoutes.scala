package com.example

import java.util.UUID.randomUUID

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route

import scala.concurrent.Future
import akka.actor.typed.ActorRef
import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.AskPattern._
import akka.util.Timeout
import com.example.Models._
import com.example.PollRegistry._

final case class PollDto(question: String, answers: Array[String], timeInMinutes: Int)
final case class AnswerDto(pollId: String, answerNumber: Int)

class PollRoutes(pollRegistry: ActorRef[PollRegistry.Command])(implicit val system: ActorSystem[_]) {

  import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
  import JsonFormats._

  private implicit val timeout: Timeout = Timeout.create(system.settings.config.getDuration("my-app.routes.ask-timeout"))

  def getPoll(id: String): Future[Option[Poll]] =
    pollRegistry.ask(GetPoll(id, _))
  def createPoll(poll: Poll): Future[String] =
    pollRegistry.ask(CreatePoll(poll, _))
  def createAnswer(answer: Answer): Future[Either[String, String]] =
    pollRegistry.ask(CreateAnswer(answer, _))
  def startPoll(id: String): Future[Either[String, _]] =
    pollRegistry.ask(StartPoll(id, _))

  private val pollRoutes1: Route = pathPrefix("polls") {
    concat(
      pathPrefix("start") {
        concat(
          pathEnd {
            concat(
              post {
                entity(as[String]) { id =>
                  onSuccess(startPoll( id )) {
                    case Right(_) => complete(StatusCodes.OK)
                    case Left(x) => complete(StatusCodes.BadGateway, x)
                  }
                }
              })
          }
        )
      },
      pathEnd {
        concat(
          post {
            entity(as[PollDto]) { p =>
              onSuccess(createPoll( Poll(randomUUID.toString, p.question, p.answers.map(x => Vote(x, 0)), None, p.timeInMinutes, randomUUID.toString) )) { id =>
                complete((StatusCodes.Created, id))
              }
            }
          })
      },
      path(Segment) { id =>
        concat(
          get {
            //#retrieve-user-info
            rejectEmptyResponse {
              onSuccess(getPoll(id)) { response =>
                complete(response)
              }
            }
          })
      })
  }
  private val pollRoutes2: Route = pathPrefix("answers") {
    concat(
      pathEnd {
        concat(
          post {
            entity(as[AnswerDto]) { a =>
              onSuccess(createAnswer( Answer(randomUUID.toString, a.pollId,  a.answerNumber, randomUUID.toString) )) { performed =>
                complete((StatusCodes.Created, performed))
              }
            }
          })
      })
  }

  val pollRoutes: Route = pollRoutes1 ~ pollRoutes2
}

