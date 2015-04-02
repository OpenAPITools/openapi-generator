package io.swagger.client.model

import io.swagger.client.core.ApiModel
import org.joda.time.DateTime


case class ExampleSearchResults (
  facets: Option[Seq[Facet]],
  examples: Option[Seq[Example]])
   extends ApiModel


