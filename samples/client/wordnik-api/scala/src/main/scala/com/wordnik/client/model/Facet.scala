package com.wordnik.client.model

import com.wordnik.client.model.FacetValue
case class Facet (
  facetValues: List[FacetValue],
  name: String)

