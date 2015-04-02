package io.swagger.client.model

import io.swagger.client.core.ApiModel
import org.joda.time.DateTime


case class Related (
  Label1: Option[String],
  RelationshipType: Option[String],
  Label2: Option[String],
  Label3: Option[String],
  Words: Option[Seq[String]],
  Gram: Option[String],
  Label4: Option[String])
   extends ApiModel


