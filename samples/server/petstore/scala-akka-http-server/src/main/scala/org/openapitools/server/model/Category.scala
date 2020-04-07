package org.openapitools.server.model


/**
 * = Pet category =
 *
 * A category for a pet
 *
 * @param id  for example: ''null''
 * @param name  for example: ''null''
*/
final case class Category (
  id: Option[Long],
  name: Option[String]
)

