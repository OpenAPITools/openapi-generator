package com.wordnik.client.model

import com.wordnik.client.model.Definition
case class DefinitionSearchResults (
  results: List[Definition],
  totalResults: Int)

