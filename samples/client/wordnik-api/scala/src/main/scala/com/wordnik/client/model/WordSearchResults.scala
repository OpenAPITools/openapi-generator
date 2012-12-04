package com.wordnik.client.model

import com.wordnik.client.model.WordSearchResult
case class WordSearchResults (
  searchResults: List[WordSearchResult],
  totalResults: Int)

