package com.example

import java.time.LocalDateTime

object Models {
  final case class Vote(answer: String, count: Int)
  final case class Poll(id: String, question: String, votes: Array[Vote], dateBegin: Option[LocalDateTime], timeInMinutes: Int, authorId: String)
  final case class Answer(id: String, pollId: String, answerNumber: Int, userId: String)
  //final case class PollResult(poll: Poll, counts: Array[Int])
}