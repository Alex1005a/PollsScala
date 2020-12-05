package com.example

import java.time.LocalDateTime

import org.mongodb.scala.bson.ObjectId

object Models {
  final case class Vote(answer: String, count: Int = 0)
  final case class Poll(_id: String, question: String, votes: List[Vote], dateBegin: Option[LocalDateTime], timeInMinutes: Int, authorId: String)
  final case class Answer(_id: String, pollId: String, answerNumber: Int, userId: String)

  object Poll {
    def apply(pollDto: PollDto, userId: String): Poll =
      Poll(new ObjectId().toHexString, pollDto.question, pollDto.answers.map(x => Vote(x)), None, pollDto.timeInMinutes, userId)
  }

  object Answer {
    def apply(answerDto: AnswerDto, userId: String): Answer =
      Answer(new ObjectId().toHexString, answerDto.pollId, answerDto.answerNumber, userId)
  }
}