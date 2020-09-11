package org.openapitools.server.model


/**
 * = Pet Tag =
 *
 * A tag for a pet
 *
 * @param id  for example: ''null''
 * @param name  for example: ''null''
*/
final case class Tag (
  id: Option[Long],
  name: Option[String]
)

