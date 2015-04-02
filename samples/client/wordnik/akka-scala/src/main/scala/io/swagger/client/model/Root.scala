package io.swagger.client.model

import io.swagger.client.core.ApiModel
import org.joda.time.DateTime


case class Root (
  Id: Option[Long],
  Name: Option[String],
  Categories: Option[Seq[Category]])
   extends ApiModel


