package io.swagger.client.model


case class WordObject(
                       id: Long,
                       word: String,
                       originalWord: String,
                       suggestions: List[String],
                       canonicalForm: String,
                       vulgar: String)
  
