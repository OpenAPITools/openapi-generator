package org.openapitools.server.model


/**
 * = a User =
 *
 * A User who is purchasing from the pet store
 *
 * @param id  for example: ''null''
 * @param username  for example: ''null''
 * @param firstName  for example: ''null''
 * @param lastName  for example: ''null''
 * @param email  for example: ''null''
 * @param password  for example: ''null''
 * @param phone  for example: ''null''
 * @param userStatus User Status for example: ''null''
*/
final case class User (
  id: Option[Long],
  username: Option[String],
  firstName: Option[String],
  lastName: Option[String],
  email: Option[String],
  password: Option[String],
  phone: Option[String],
  userStatus: Option[Int]
)

