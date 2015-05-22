package io.swagger.client.model

import io.swagger.client.core.ApiModel
import org.joda.time.DateTime


case class Facet (
  facetValues: Option[Seq[FacetValue]],
  name: Option[String])
   extends ApiModel


