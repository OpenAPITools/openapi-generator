package io.swagger.client.model

import io.swagger.client.core.ApiModel
import org.joda.time.DateTime


case class Facet (
  FacetValues: Option[Seq[FacetValue]],
  Name: Option[String])
   extends ApiModel


