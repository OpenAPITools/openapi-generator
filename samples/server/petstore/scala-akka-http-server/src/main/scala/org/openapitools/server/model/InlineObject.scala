package org.openapitools.server.model


/**
 * @param name Updated name of the pet for example: ''null''
 * @param status Updated status of the pet for example: ''null''
*/
final case class InlineObject (
  name: Option[String],
  status: Option[String]
)

