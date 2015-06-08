package io.swagger.client.model


case class Example(
                    id: Long,
                    exampleId: Long,
                    title: String,
                    text: String,
                    score: ScoredWord,
                    sentence: Sentence,
                    word: String,
                    provider: ContentProvider,
                    year: Integer,
                    rating: Float,
                    documentId: Long,
                    url: String)
  
