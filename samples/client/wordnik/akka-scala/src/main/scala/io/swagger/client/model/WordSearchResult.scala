package io.swagger.client.model

import io.swagger.client.core.ApiModel
import org.joda.time.DateTime


case class WordSearchResult (
  count: Option[Long],
  lexicality: Option[Double],
  word: Option[String])
   extends ApiModel


