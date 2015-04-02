package io.swagger.client.model

import io.swagger.client.core.ApiModel
import org.joda.time.DateTime


case class WordSearchResults (
  SearchResults: Option[Seq[WordSearchResult]],
  TotalResults: Option[Int])
   extends ApiModel


