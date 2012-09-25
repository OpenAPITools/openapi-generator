package com.wordnik.client.model

import java.util.Date
case class AudioFile (
  attributionUrl: String,
  commentCount: Int,
  voteCount: Int,
  fileUrl: String,
  audioType: String,
  id: Long,
  duration: Double,
  attributionText: String,
  createdBy: String,
  description: String,
  createdAt: Date,
  voteWeightedAverage: Float,
  voteAverage: Float,
  word: String)

