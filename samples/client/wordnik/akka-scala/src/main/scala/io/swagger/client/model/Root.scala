package io.swagger.client.model

import io.swagger.client.core.ApiModel
import org.joda.time.DateTime


case class Root (
  id: Option[Long],
  name: Option[String],
  categories: Option[Seq[Category]])
   extends ApiModel


