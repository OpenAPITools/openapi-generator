package io.swagger.client.api

import io.swagger.client.model.User
import io.swagger.client._
import scala.concurrent.{ Future, Await }
import scala.concurrent.duration._
import collection.mutable

class UserApi(client: TransportClient, config: SwaggerConfig) extends ApiClient(client, config) {

  
  def createUser(body: Option[User] = None
      )(implicit reader: ClientResponseReader[Unit], writer: RequestWriter[User]): Future[Unit] = {
    // create path and map variables
    val path = (addFmt("/user"))

    // query params
    val queryParams = new mutable.HashMap[String, String]
    val headerParams = new mutable.HashMap[String, String]

    

    

    

    val resFuture = client.submit("POST", path, queryParams.toMap, headerParams.toMap, writer.write(body))
    resFuture flatMap { resp =>
      process(reader.read(resp))
    }
  }

  
  def createUsersWithArrayInput(body: Option[List[User]] = None
      )(implicit reader: ClientResponseReader[Unit], writer: RequestWriter[List[User]]): Future[Unit] = {
    // create path and map variables
    val path = (addFmt("/user/createWithArray"))

    // query params
    val queryParams = new mutable.HashMap[String, String]
    val headerParams = new mutable.HashMap[String, String]

    

    

    

    val resFuture = client.submit("POST", path, queryParams.toMap, headerParams.toMap, writer.write(body))
    resFuture flatMap { resp =>
      process(reader.read(resp))
    }
  }

  
  def createUsersWithListInput(body: Option[List[User]] = None
      )(implicit reader: ClientResponseReader[Unit], writer: RequestWriter[List[User]]): Future[Unit] = {
    // create path and map variables
    val path = (addFmt("/user/createWithList"))

    // query params
    val queryParams = new mutable.HashMap[String, String]
    val headerParams = new mutable.HashMap[String, String]

    

    

    

    val resFuture = client.submit("POST", path, queryParams.toMap, headerParams.toMap, writer.write(body))
    resFuture flatMap { resp =>
      process(reader.read(resp))
    }
  }

  
  def loginUser(username: Option[String] = None,
      password: Option[String] = None
      )(implicit reader: ClientResponseReader[String]): Future[String] = {
    // create path and map variables
    val path = (addFmt("/user/login"))

    // query params
    val queryParams = new mutable.HashMap[String, String]
    val headerParams = new mutable.HashMap[String, String]

    

    if(username != null) username.foreach { v => queryParams += "username" -> v.toString }if(password != null) password.foreach { v => queryParams += "password" -> v.toString }

    

    val resFuture = client.submit("GET", path, queryParams.toMap, headerParams.toMap, "")
    resFuture flatMap { resp =>
      process(reader.read(resp))
    }
  }

  
  def logoutUser()(implicit reader: ClientResponseReader[Unit]): Future[Unit] = {
    // create path and map variables
    val path = (addFmt("/user/logout"))

    // query params
    val queryParams = new mutable.HashMap[String, String]
    val headerParams = new mutable.HashMap[String, String]

    

    

    

    val resFuture = client.submit("GET", path, queryParams.toMap, headerParams.toMap, "")
    resFuture flatMap { resp =>
      process(reader.read(resp))
    }
  }

  
  def getUserByName(username: String)(implicit reader: ClientResponseReader[User]): Future[User] = {
    // create path and map variables
    val path = (addFmt("/user/{username}")
        replaceAll ("\\{" + "username" + "\\}",username.toString))

    // query params
    val queryParams = new mutable.HashMap[String, String]
    val headerParams = new mutable.HashMap[String, String]

    

    

    

    val resFuture = client.submit("GET", path, queryParams.toMap, headerParams.toMap, "")
    resFuture flatMap { resp =>
      process(reader.read(resp))
    }
  }

  
  def updateUser(username: String,
      body: Option[User] = None
      )(implicit reader: ClientResponseReader[Unit], writer: RequestWriter[User]): Future[Unit] = {
    // create path and map variables
    val path = (addFmt("/user/{username}")
        replaceAll ("\\{" + "username" + "\\}",username.toString))

    // query params
    val queryParams = new mutable.HashMap[String, String]
    val headerParams = new mutable.HashMap[String, String]

    

    

    

    val resFuture = client.submit("PUT", path, queryParams.toMap, headerParams.toMap, writer.write(body))
    resFuture flatMap { resp =>
      process(reader.read(resp))
    }
  }

  
  def deleteUser(username: String)(implicit reader: ClientResponseReader[Unit]): Future[Unit] = {
    // create path and map variables
    val path = (addFmt("/user/{username}")
        replaceAll ("\\{" + "username" + "\\}",username.toString))

    // query params
    val queryParams = new mutable.HashMap[String, String]
    val headerParams = new mutable.HashMap[String, String]

    

    

    

    val resFuture = client.submit("DELETE", path, queryParams.toMap, headerParams.toMap, "")
    resFuture flatMap { resp =>
      process(reader.read(resp))
    }
  }

  

}
