package org.openapitools.server.model


/**
 * = a Pet =
 *
 * A pet for sale in the pet store
 *
 * @param id  for example: ''null''
 * @param category  for example: ''null''
 * @param name  for example: ''doggie''
 * @param photoUrls  for example: ''null''
 * @param tags  for example: ''null''
 * @param status pet status in the store for example: ''null''
*/
final case class Pet (
  id: Option[Long],
  category: Option[Category],
  name: String,
  photoUrls: Seq[String],
  tags: Option[Seq[Tag]],
  status: Option[String]
)

