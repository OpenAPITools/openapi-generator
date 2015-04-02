package io.swagger.client.model

import io.swagger.client.core.ApiModel
import org.joda.time.DateTime


case class WordSearchResults (
  searchResults: Option[Seq[WordSearchResult]],
  totalResults: Option[Int])
   extends ApiModel


