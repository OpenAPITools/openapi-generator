package io.swagger.client.model

import io.swagger.client.core.ApiModel
import org.joda.time.DateTime


case class Bigram (
  count: Option[Long],
  gram2: Option[String],
  gram1: Option[String],
  wlmi: Option[Double],
  mi: Option[Double])
   extends ApiModel


