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
  final case class StartPoll(id: String, userId: String, replyTo: ActorRef[Either[String, _]]) extends Command

  def apply(): Behavior[Command] = registry()

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

  private def validatePollForStart(pollOpt: Option[Poll], userId: String): Either[String, Poll] = for {
    p <- validatePollIsEmpty(pollOpt)
    _ <- validatePollIsNotStart(p)
    _ <- if(p.authorId != userId) Left("User is not author this poll") else Right()
  } yield p

  private def validatePollForAnswer(pollOpt: Option[Poll], a: Answer): Either[String, _] = for {
    p <- validatePollIsEmpty(pollOpt)
    date <- validatePollIsStart(p)
    _ <- validatePollIsNotFinish(p, date)
    _ <- validateAnswerNumber(a, p)
    _ <- if(AnswerRepository.existWithUserId(a.userId)) Left("Answer with this user id exist") else Right()
  } yield Right()

  private def registry(): Behavior[Command] =
    Behaviors.receiveMessage {
      case CreatePoll(poll, replyTo) =>
        replyTo ! PollRepository.create(poll).fold(a => a, b => b)
        Behaviors.same

      case GetPoll(id, replyTo) =>
        val poll = PollRepository.getById(id)
        replyTo ! poll
        Behaviors.same

      case CreateAnswer(a, replyTo) =>
        val p = PollRepository.getById(a.pollId)
        val res = validatePollForAnswer(p, a)
        if(res.isLeft) {
          replyTo ! res.map(_ => "")
          Behaviors.same
        }
        else {
          val poll = p.get
          val votes = poll.votes.zipWithIndex.map{ case (x, i) =>
            if(i == a.answerNumber) x.copy(count = x.count + 1)
            else x
          }

          PollRepository.update(poll.copy(votes = votes))
          val answerCreateRes = AnswerRepository.create(a)
          replyTo ! answerCreateRes

          Behaviors.same
        }

      case StartPoll(id, userId, replyTo) =>
        val poll = PollRepository.getById(id)
        val res = validatePollForStart(poll, userId)
        replyTo ! res
        res match {
          case Left(_) => Behaviors.same
          case Right(p) =>
            PollRepository.update(p.copy(dateBegin = Some(LocalDateTime.now())))
            Behaviors.same
        }
    }
}
