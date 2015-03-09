package io.swagger.client.model

import org.joda.time.DateTime



case class AudioFile (
  attributionUrl: String,
  commentCount: Integer,
  voteCount: Integer,
  fileUrl: String,
  audioType: String,
  id: Long,
  duration: Double,
  attributionText: String,
  createdBy: String,
  description: String,
  createdAt: DateTime,
  voteWeightedAverage: Float,
  voteAverage: Float,
  word: String)
  
