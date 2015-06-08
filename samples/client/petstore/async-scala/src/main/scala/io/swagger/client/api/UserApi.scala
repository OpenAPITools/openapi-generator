package io.swagger.client.api

import io.swagger.client.model.User
import io.swagger.client._
import scala.concurrent.{ Future, Await }
import scala.concurrent.duration._
import collection.mutable

    class UserApi(client: TransportClient, config: SwaggerConfig) extends ApiClient(client, config) {

    
        def createUser(body: User)(implicit reader: ClientResponseReader[Unit], writer: RequestWriter[User]): Future[Unit] = {
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

    
        def createUsersWithArrayInput(body: List[User])(implicit reader: ClientResponseReader[Unit], writer: RequestWriter[List[User]]): Future[Unit] = {
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

    
        def createUsersWithListInput(body: List[User])(implicit reader: ClientResponseReader[Unit], writer: RequestWriter[List[User]]): Future[Unit] = {
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

    
        def loginUser(username: String,
        password: String)(implicit reader: ClientResponseReader[String]): Future[String] = {
        // create path and map variables
        val path = (addFmt("/user/login"))

        // query params
        val queryParams = new mutable.HashMap[String, String]
        val headerParams = new mutable.HashMap[String, String]

        

        
            if(username != null)   queryParams += "username" -> username.toString
            if(password != null)   queryParams += "password" -> password.toString

        

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
        body: User)(implicit reader: ClientResponseReader[Unit], writer: RequestWriter[User]): Future[Unit] = {
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
