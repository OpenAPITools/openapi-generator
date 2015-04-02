package io.swagger.client.model

import io.swagger.client.core.ApiModel
import org.joda.time.DateTime


case class DefinitionSearchResults (
  Results: Option[Seq[Definition]],
  TotalResults: Option[Int])
   extends ApiModel


