package io.swagger.client.model

import io.swagger.client.core.ApiModel
import org.joda.time.DateTime


case class SimpleExample (
  id: Option[Long],
  title: Option[String],
  text: Option[String],
  url: Option[String])
   extends ApiModel


