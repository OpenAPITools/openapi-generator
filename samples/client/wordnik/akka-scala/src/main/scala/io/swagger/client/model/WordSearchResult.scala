package io.swagger.client.model

import io.swagger.client.core.ApiModel
import org.joda.time.DateTime


case class WordSearchResult (
  Count: Option[Long],
  Lexicality: Option[Double],
  Word: Option[String])
   extends ApiModel


