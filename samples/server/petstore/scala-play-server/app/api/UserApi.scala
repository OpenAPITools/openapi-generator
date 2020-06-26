package api

import model.User


trait UserApi {
  /**
    * Create user
    * This can only be done by the logged in user.
    * @param user Created user object
    */
  def createUser(user: User): Unit

  /**
    * Creates list of users with given input array
    * @param user List of user object
    */
  def createUsersWithArrayInput(user: List[User]): Unit

  /**
    * Creates list of users with given input array
    * @param user List of user object
    */
  def createUsersWithListInput(user: List[User]): Unit

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
    * @param user Updated user object
    */
  def updateUser(username: String, user: User): Unit
}
