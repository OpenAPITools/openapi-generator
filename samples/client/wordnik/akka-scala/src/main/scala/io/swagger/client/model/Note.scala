package io.swagger.client.model

import io.swagger.client.core.ApiModel
import org.joda.time.DateTime


case class Note (
  NoteType: Option[String],
  AppliesTo: Option[Seq[String]],
  Value: Option[String],
  Pos: Option[Int])
   extends ApiModel


