package io.swagger.client.model

import io.swagger.client.core.ApiModel
import org.joda.time.DateTime


case class Bigram (
  Count: Option[Long],
  Gram2: Option[String],
  Gram1: Option[String],
  Wlmi: Option[Double],
  Mi: Option[Double])
   extends ApiModel


