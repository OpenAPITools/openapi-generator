package api

import model.User

@javax.annotation.Generated(value = Array("org.openapitools.codegen.languages.ScalaPlayFrameworkServerCodegen"), date = "2019-03-26T16:21:58.590+08:00[Asia/Hong_Kong]")
trait UserApi {
  /**
    * Create user
    * This can only be done by the logged in user.
    * @param body Created user object
    */
  def createUser(body: User): Unit

  /**
    * Creates list of users with given input array
    * @param body List of user object
    */
  def createUsersWithArrayInput(body: List[User]): Unit

  /**
    * Creates list of users with given input array
    * @param body List of user object
    */
  def createUsersWithListInput(body: List[User]): Unit

  /**
    * Delete user
    * This can only be done by the logged in user.
    * @param username The name that needs to be deleted
    */
  def deleteUser(username: String): Unit

  /**
    * Get user by user name
    * @param username The name that needs to be fetched. Use user1 for testing.
    */
  def getUserByName(username: String): User

  /**
    * Logs user into the system
    * @param username The user name for login
    * @param password The password for login in clear text
    */
  def loginUser(username: String, password: String): String

  /**
    * Logs out current logged in user session
    */
  def logoutUser(): Unit

  /**
    * Updated user
    * This can only be done by the logged in user.
    * @param username name that need to be deleted
    * @param body Updated user object
    */
  def updateUser(username: String, body: User): Unit
}
