package api

import model.User

/**
  * Provides a default implementation for [[UserApi]].
  */
@javax.annotation.Generated(value = Array("org.openapitools.codegen.languages.ScalaPlayFrameworkServerCodegen"), date = "2019-03-26T02:45:22.426+02:00[Asia/Jerusalem]")
class UserApiImpl extends UserApi {
  /**
    * @inheritdoc
    */
  override def createUser(body: User): Unit = {
    // TODO: Implement better logic

    
  }

  /**
    * @inheritdoc
    */
  override def createUsersWithArrayInput(body: List[User]): Unit = {
    // TODO: Implement better logic

    
  }

  /**
    * @inheritdoc
    */
  override def createUsersWithListInput(body: List[User]): Unit = {
    // TODO: Implement better logic

    
  }

  /**
    * @inheritdoc
    */
  override def deleteUser(username: String): Unit = {
    // TODO: Implement better logic

    
  }

  /**
    * @inheritdoc
    */
  override def getUserByName(username: String): User = {
    // TODO: Implement better logic

    User(None, None, None, None, None, None, None, None)
  }

  /**
    * @inheritdoc
    */
  override def loginUser(username: String, password: String): String = {
    // TODO: Implement better logic

    ""
  }

  /**
    * @inheritdoc
    */
  override def logoutUser(): Unit = {
    // TODO: Implement better logic

    
  }

  /**
    * @inheritdoc
    */
  override def updateUser(username: String, body: User): Unit = {
    // TODO: Implement better logic

    
  }
}
