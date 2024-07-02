package api

import java.time.OffsetDateTime
import model.User

/**
  * Provides a default implementation for [[UserApi]].
  */
@javax.annotation.Generated(value = Array("org.openapitools.codegen.languages.ScalaPlayFrameworkServerCodegen"), comments = "Generator version: 7.5.0-SNAPSHOT")
class UserApiImpl extends UserApi {
  /**
    * @inheritdoc
    */
  override def createUser(user: User): Unit = {
    // TODO: Implement better logic

    
  }

  /**
    * @inheritdoc
    */
  override def createUsersWithArrayInput(user: List[User]): Unit = {
    // TODO: Implement better logic

    
  }

  /**
    * @inheritdoc
    */
  override def createUsersWithListInput(user: List[User]): Unit = {
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
  override def updateUser(username: String, user: User): Unit = {
    // TODO: Implement better logic

    
  }
}
