package io.swagger.client.model

import io.swagger.client.core.ApiModel
import org.joda.time.DateTime


case class ExampleSearchResults (
  Facets: Option[Seq[Facet]],
  Examples: Option[Seq[Example]])
   extends ApiModel


