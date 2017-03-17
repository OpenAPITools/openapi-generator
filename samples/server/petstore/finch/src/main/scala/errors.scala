package io.swagger

/**
 * The parent error from which most PetstoreAPI errors extend. Thrown whenever something in the api goes wrong.
 */
abstract class PetstoreError(msg: String) extends Exception(msg) {
  def message: String
}

/**
 * Thrown when the object given is invalid
 * @param message An error message
 */
case class InvalidInput(message: String) extends PetstoreError(message)

/**
 * Thrown when the given object is missing a unique ID.
 * @param message An error message
 */
case class MissingIdentifier(message: String) extends PetstoreError(message)

/**
 * Thrown when the given record does not exist in the database.
 * @param message An error message
 */
case class RecordNotFound(message: String) extends PetstoreError(message)

