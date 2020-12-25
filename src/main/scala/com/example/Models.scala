package com.example

import java.time.LocalDateTime

import org.mongodb.scala.bson.ObjectId

object Models {
  final case class User(_id: String, name: String, password: String)
  final case class Vote(answer: String, count: Int = 0)
  final case class Poll(id: String, question: String, votes: List[Vote], dateBegin: Option[LocalDateTime], timeInMinutes: Int, authorId: String)
  final case class Answer(id: String, pollId: String, answerNumber: Int, userId: String)

  object Poll {
    def apply(pollDto: PollDto, userId: String): Poll = {
      Poll(new ObjectId().toHexString, pollDto.question, pollDto.answers.map(x => Vote(x)), None, pollDto.timeInMinutes, userId)
    }
  }

  object Answer {
    def apply(answerDto: AnswerDto, userId: String): Answer =
      Answer(new ObjectId().toHexString, answerDto.pollId, answerDto.answerNumber, userId)
  }
  object User {
    def apply(name: String, password: String): User =
      User(new ObjectId().toHexString, name, password)
  }
}
