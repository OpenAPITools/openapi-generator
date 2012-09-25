package com.wordnik.client.model

import com.wordnik.client.model.Facet
import com.wordnik.client.model.Example
case class ExampleSearchResults (
  facets: List[Facet],
  examples: List[Example])

