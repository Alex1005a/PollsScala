package com.example

import java.time.LocalDateTime

import akka.actor.typed.ActorRef
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import com.example.Models._

object PollRegistry {
  sealed trait Command
  final case class CreatePoll(poll: Poll, replyTo: ActorRef[String]) extends Command
  final case class CreateAnswer(user: Answer, replyTo: ActorRef[Either[String, String]]) extends Command
  final case class GetPoll(id: String, replyTo: ActorRef[Option[Poll]]) extends Command
  final case class StartPoll(id: String, replyTo: ActorRef[Either[String, _]]) extends Command

  def apply(): Behavior[Command] = registry(Set.empty, Set.empty)
  /*
  private def getCountArray(poll: Poll, answers: Set[Answer]): Array[Int] = {
    poll.votes.zipWithIndex.map{ case (_, index) =>
      answers.count(_.answerNumber == index)
    }
  }
  */
  private def validatePollIsEmpty(pollOpt: Option[Poll]): Either[String, Poll] ={
    pollOpt.map(x => Right(x)).getOrElse(Left("Poll not found"))
  }
  private def validatePollIsStart(poll: Poll): Either[String, LocalDateTime] ={
    poll.dateBegin.map(x => Right(x)).getOrElse(Left("Poll not start"))
  }
  private def validatePollIsNotStart(poll: Poll): Either[String, _] ={
    if(poll.dateBegin.isDefined) return Left("Poll is already start")
    Right()
  }
  private def validatePollIsNotFinish(poll: Poll, dateBegin: LocalDateTime): Either[String, _] ={
    if(dateBegin.plusMinutes(poll.timeInMinutes).isBefore(LocalDateTime.now())) return Left("Poll finish")
    Right()
  }
  private def validateAnswerNumber(a: Answer, p: Poll): Either[String, _] ={
    if(a.answerNumber >= p.votes.length | a.answerNumber < 0) return Left("Answer number not valid")
    Right()
  }

  private def validatePollForStart(pollOpt: Option[Poll]): Either[String, Poll] = for {
    p <- validatePollIsEmpty(pollOpt)
    _ <- validatePollIsNotStart(p)
  } yield p

  private def validatePollForAnswer(pollOpt: Option[Poll], a: Answer): Either[String, _] = for {
    p <- validatePollIsEmpty(pollOpt)
    date <- validatePollIsStart(p)
    _ <- validatePollIsNotFinish(p, date)
    _ <- validateAnswerNumber(a, p)
  } yield Right()

  /*
  private def validatePollForStart(pollOpt: Option[Poll]): Either[String, Poll] = {
    if(pollOpt.isEmpty) return Left("Poll not found")
    val poll = pollOpt.get
    if(poll.dateBegin.isDefined) return Left("Poll is already start")
    Right(poll)
  }

  private def validatePollForAnswer(pollOpt: Option[Poll], a: Answer): Either[String, _] = {
    if(pollOpt.isEmpty) return Left("Poll not found")
    val p = pollOpt.get
    if(p.dateBegin.isEmpty) return Left("Poll not start")
    val dateBegin = p.dateBegin.get
    if(dateBegin.plusMinutes(p.timeInMinutes).isBefore(LocalDateTime.now())) return Left("Poll finish")
    if(a.answerNumber >= p.answers.length | a.answerNumber < 0) return Left("Answer number not valid")
    Right()
  }
  */

  private def registry(polls: Set[Poll], answers: Set[Answer]): Behavior[Command] =
    Behaviors.receiveMessage {
      case CreatePoll(poll, replyTo) =>
        replyTo ! poll.id
        registry(polls + poll, answers)
      case GetPoll(id, replyTo) =>
        val poll = polls.find(_.id == id)
        replyTo ! poll
        Behaviors.same
      case CreateAnswer(a, replyTo) =>
        val p = polls.find(_.id == a.pollId)
        val res = validatePollForAnswer(p, a)
        replyTo ! res.map(_ => a.id)
        res match {
          case Left(_) => Behaviors.same
          case Right(_) =>
            val poll = p.get
            val votes = poll.votes.zipWithIndex.map{ case (x, i) =>
              if(i == a.answerNumber) x.copy(count = x.count + 1)
              else x
            }
            registry(polls.filterNot(_.id == poll.id) + poll.copy(votes = votes), answers.filterNot(_.userId == a.userId) + a)
        }
      case StartPoll(id, replyTo) =>
        val poll = polls.find(_.id == id)
        val res = validatePollForStart(poll)
        replyTo ! res
        res match {
          case Left(_) => Behaviors.same
          case Right(p) => registry(polls.filterNot(_.id == id) + p.copy(dateBegin = Some(LocalDateTime.now())), answers)
        }

    }
}
