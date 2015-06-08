package io.swagger.client.model


case class WordOfTheDay(
                         id: Option[Long],
                         parentId: Option[String],
                         category: Option[String],
                         createdBy: Option[String],
                         createdAt: Option[DateTime],
                         contentProvider: Option[ContentProvider],
                         htmlExtra: Option[String],
                         word: Option[String],
                         definitions: Option[Seq[SimpleDefinition]],
                         examples: Option[Seq[SimpleExample]],
                         note: Option[String],
                         publishDate: Option[DateTime])
  extends ApiModel


