package com.wordnik.client.model

import com.wordnik.client.model.WordSearchResult
case class WordSearchResults (
  totalResults: Int,
  searchResults: List[WordSearchResult])

