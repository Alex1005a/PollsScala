package com.example

import java.time.LocalDateTime
import com.example.Models.{Answer, Poll, User, Vote}
import org.mongodb.scala.bson.codecs.Macros._
import org.mongodb.scala.MongoClient.DEFAULT_CODEC_REGISTRY
import org.bson.codecs.configuration.CodecRegistries.{fromProviders, fromRegistries}
import org.mongodb.scala.bson.ObjectId
import org.mongodb.scala.model.Filters.equal
import org.mongodb.scala.{MongoClient, MongoCollection, MongoDatabase}

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.{Either, Right}

private final case class PollMongo(_id: ObjectId, question: String, votes: List[Vote], dateBegin: Option[LocalDateTime], timeInMinutes: Int, authorId: String)
private object PollMongo {
  def apply(poll: Poll): PollMongo = {
    PollMongo(new ObjectId(), poll.question, poll.votes, poll.dateBegin, poll.timeInMinutes, poll.authorId)
  }
  def apply(poll: Poll, objectId: ObjectId): PollMongo = {
    PollMongo(objectId, poll.question, poll.votes, poll.dateBegin, poll.timeInMinutes, poll.authorId)
  }
}
private final case class AnswerMongo(_id: ObjectId, pollId: String, answerNumber: Int, userId: String)
private object AnswerMongo {
  def apply(answer: Answer): AnswerMongo = {
    AnswerMongo(new ObjectId(), answer.pollId, answer.answerNumber, answer.userId)
  }
}

object PollRepository {
  private def getCollection: MongoCollection[PollMongo] = {
    val codeRegistry = fromRegistries(fromProviders(classOf[PollMongo], classOf[Vote]), DEFAULT_CODEC_REGISTRY)
    val mongoClient: MongoClient = MongoClient("mongodb://localhost:27017")
    val database: MongoDatabase = mongoClient.getDatabase("scalaPoll").withCodecRegistry(codeRegistry)
    database.getCollection("polls")
  }

  def create(poll: Poll): Either[String, String] = {
    val p = PollMongo(poll)
    Await.result(getCollection.insertOne(p).toFuture, Duration.Inf)
    Right(p._id.toHexString)
  }

  def getById(id: String): Option[Poll] = {
    val res = Await.result(getCollection.find(equal("_id", new ObjectId(id))).toFuture, Duration.Inf)
    res.headOption.map(p => Poll(p._id.toHexString,
                                 p.question,
                                 p.votes,
                                 p.dateBegin,
                                 p.timeInMinutes,
                                 p.authorId))
  }

  def update(poll: Poll): Either[String, _] = {
    val objectId = new ObjectId(poll.id)
    val res = Await.result(getCollection.replaceOne(equal("_id", objectId), PollMongo(poll, objectId)).toFuture, Duration.Inf)
    if(res.wasAcknowledged()) Right()
    else Left(res.toString)
  }
}

object AnswerRepository {
  private def getCollection: MongoCollection[AnswerMongo] = {
    val codeRegistry = fromRegistries(fromProviders(classOf[AnswerMongo]), DEFAULT_CODEC_REGISTRY)
    val mongoClient: MongoClient = MongoClient("mongodb://localhost:27017")
    val database: MongoDatabase = mongoClient.getDatabase("scalaPoll").withCodecRegistry(codeRegistry)
    database.getCollection("answers")
  }

  def create(answer: Answer): Either[String, String] = {
    val a = AnswerMongo(answer)
    Await.result(getCollection.insertOne(a).toFuture, Duration.Inf)
    Right(a._id.toHexString)
  }

  def existWithUserId(userId: String): Boolean = {
    Await.result(getCollection.countDocuments(equal("userId", userId)).toFuture, Duration.Inf) != 0
  }
}

object UserRepository {
  private def getCollection: MongoCollection[User] = {
    val codeRegistry = fromRegistries(fromProviders(classOf[User]), DEFAULT_CODEC_REGISTRY)
    val mongoClient: MongoClient = MongoClient("mongodb://localhost:27017")
    val database: MongoDatabase = mongoClient.getDatabase("scalaPoll").withCodecRegistry(codeRegistry)
    database.getCollection("users")
  }

  def create(user: User): Either[String, String] = {
    Await.result(getCollection.insertOne(user).toFuture, Duration.Inf)
    Right(user._id)
  }

  def getByName(username: String): Option[User] = {
    val res = Await.result(getCollection.find(equal("name", username)).toFuture, Duration.Inf)
    res.headOption
  }
}