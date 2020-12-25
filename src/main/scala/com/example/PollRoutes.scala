package com.example

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route

import scala.concurrent.Future
import akka.actor.typed.ActorRef
import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.AskPattern._
import akka.http.scaladsl.server.directives.Credentials
import akka.util.Timeout
import com.example.Models._
import com.example.PollRegistry._

final case class PollDto(question: String, answers: List[String], timeInMinutes: Int)
final case class AnswerDto(pollId: String, answerNumber: Int)
final case class UserDto(name: String, password: String)

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
  def startPoll(id: String, userId: String): Future[Either[String, _]] =
    pollRegistry.ask(StartPoll(id, userId, _))


  private def myUserPassAuthenticator(credentials: Credentials): Option[String] =
    credentials match {
      case p @ Credentials.Provided(name) =>
        val userOption = UserRepository.getByName(name)
        if(userOption.isEmpty) return None
        if(p.verify(userOption.get.password)) return Some(userOption.get._id)
        None
      case _ => None
    }

  private val pollRoutes1: Route = pathPrefix("polls") {
    concat(
      pathPrefix("start") {
        concat(
          pathEnd {
            concat(
              post {
                authenticateBasic(realm = "secure", myUserPassAuthenticator) { userId =>
                  entity(as[String]) { id =>
                    onSuccess(startPoll(id, userId)) {
                      case Right(_) => complete(StatusCodes.OK)
                      case Left(x) => complete(StatusCodes.BadGateway, x)
                    }
                  }
                }
              })
          }
        )
      },
      pathEnd {
        concat(
          post {
            authenticateBasic(realm = "secure", myUserPassAuthenticator) { id =>
              entity(as[PollDto]) { p =>
                onSuccess(createPoll(Poll(p, id))) { id =>
                  complete((StatusCodes.Created, id))
                }
              }
            }
          })
      },
      path(Segment) { id =>
        concat(
          get {
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
            authenticateBasic(realm = "secure", myUserPassAuthenticator) { id =>
              entity(as[AnswerDto]) { a =>
                onSuccess(createAnswer( Answer(a, id) )) { performed =>
                  complete((StatusCodes.Created, performed))
                }
              }
            }
          })
      })
  }

  private val pollRoutes3: Route = pathPrefix("user") {
    concat(
      pathEnd {
        concat(
          post {
            entity(as[UserDto]) { u =>
              UserRepository.create(User(u.name, u.password)) match {
                case Right(_) => complete(StatusCodes.OK)
                case Left(x) => complete(StatusCodes.BadGateway, x)
              }
            }
          })
      })
  }

  val pollRoutes: Route = pollRoutes1 ~ pollRoutes2 ~ pollRoutes3
}

