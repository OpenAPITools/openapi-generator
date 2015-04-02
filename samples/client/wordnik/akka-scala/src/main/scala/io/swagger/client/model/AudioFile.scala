package io.swagger.client.model

import io.swagger.client.core.ApiModel
import org.joda.time.DateTime


case class AudioFile (
  AttributionUrl: Option[String],
  CommentCount: Option[Int],
  VoteCount: Option[Int],
  FileUrl: Option[String],
  AudioType: Option[String],
  Id: Option[Long],
  Duration: Option[Double],
  AttributionText: Option[String],
  CreatedBy: Option[String],
  Description: Option[String],
  CreatedAt: Option[DateTime],
  VoteWeightedAverage: Option[Float],
  VoteAverage: Option[Float],
  Word: Option[String])
   extends ApiModel


