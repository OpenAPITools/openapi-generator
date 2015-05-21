package io.swagger.client.model

import io.swagger.client.core.ApiModel
import org.joda.time.DateTime


case class DefinitionSearchResults (
  results: Option[Seq[Definition]],
  totalResults: Option[Int])
   extends ApiModel


