package io.swagger.client.model


case class Definition(
                       extendedText: String,
                       text: String,
                       sourceDictionary: String,
                       citations: List[Citation],
                       labels: List[Label],
                       score: Float,
                       exampleUses: List[ExampleUsage],
                       attributionUrl: String,
                       seqString: String,
                       attributionText: String,
                       relatedWords: List[Related],
                       sequence: String,
                       word: String,
                       notes: List[Note],
                       textProns: List[TextPron],
                       partOfSpeech: String)
  
