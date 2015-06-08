package io.swagger.client.model


case class WordOfTheDay(
                         id: Long,
                         parentId: String,
                         category: String,
                         createdBy: String,
                         createdAt: DateTime,
                         contentProvider: ContentProvider,
                         htmlExtra: String,
                         word: String,
                         definitions: List[SimpleDefinition],
                         examples: List[SimpleExample],
                         note: String,
                         publishDate: DateTime)
  
