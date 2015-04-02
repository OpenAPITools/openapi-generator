package io.swagger.client.model

import io.swagger.client.core.ApiModel
import org.joda.time.DateTime


case class Note (
  noteType: Option[String],
  appliesTo: Option[Seq[String]],
  value: Option[String],
  pos: Option[Int])
   extends ApiModel


