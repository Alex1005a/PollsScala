package com.example

import com.example.Models.{Answer, Poll, Vote}
import org.mongodb.scala.bson.codecs.Macros._
import org.mongodb.scala.MongoClient.DEFAULT_CODEC_REGISTRY
import org.bson.codecs.configuration.CodecRegistries.{fromProviders, fromRegistries}
import org.mongodb.scala.model.Filters.equal
import org.mongodb.scala.{MongoClient, MongoCollection, MongoDatabase}

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.{Either, Right}


object PollRepository {
  private def getCollection: MongoCollection[Poll] = {
    val codeRegistry = fromRegistries(fromProviders(classOf[Poll], classOf[Vote]), DEFAULT_CODEC_REGISTRY)
    val mongoClient: MongoClient = MongoClient("mongodb://localhost:27017")
    val database: MongoDatabase = mongoClient.getDatabase("scalaPoll").withCodecRegistry(codeRegistry)
    database.getCollection("polls")
  }

  def create(poll: Poll): Either[String, String] = {
    Await.result(getCollection.insertOne(poll).toFuture, Duration.Inf)
    Right(poll._id)
  }

  def getById(id: String): Option[Poll] = {
    val res = Await.result(getCollection.find(equal("_id", id)).toFuture, Duration.Inf)
    res.headOption
  }

  def update(poll: Poll): Either[String, _] = {
    val res = Await.result(getCollection.replaceOne(equal("_id", poll._id), poll).toFuture, Duration.Inf)
    if(res.wasAcknowledged()) Right()
    else Left(res.toString)
  }
}

object AnswerRepository {
  private def getCollection: MongoCollection[Answer] = {
    val codeRegistry = fromRegistries(fromProviders(classOf[Answer]), DEFAULT_CODEC_REGISTRY)
    val mongoClient: MongoClient = MongoClient("mongodb://localhost:27017")
    val database: MongoDatabase = mongoClient.getDatabase("scalaPoll").withCodecRegistry(codeRegistry)
    database.getCollection("answers")
  }

  def create(answer: Answer): Either[String, String] = {
    Await.result(getCollection.insertOne(answer).toFuture, Duration.Inf)
    Right(answer._id)
  }
}