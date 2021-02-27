package org.openapitools.apis

import java.io._
import org.openapitools._
import org.openapitools.models._
import scala.collection.immutable.Seq
import org.openapitools.models.User
import io.finch.circe._
import io.circe.generic.semiauto._
import com.twitter.concurrent.AsyncStream
import com.twitter.finagle.Service
import com.twitter.finagle.Http
import com.twitter.finagle.http.{Request, Response}
import com.twitter.finagle.http.exp.Multipart.{FileUpload, InMemoryFileUpload, OnDiskFileUpload}
import com.twitter.util.Future
import com.twitter.io.Buf
import io.finch._, items._
import java.io.File
import java.nio.file.Files
import java.time._

object UserApi {
    /**
    * Compiles all service endpoints.
    * @return Bundled compilation of all service endpoints.
    */
    def endpoints(da: DataAccessor) =
        createUser(da) :+:
        createUsersWithArrayInput(da) :+:
        createUsersWithListInput(da) :+:
        deleteUser(da) :+:
        getUserByName(da) :+:
        loginUser(da) :+:
        logoutUser(da) :+:
        updateUser(da)


    private def checkError(e: CommonError) = e match {
      case InvalidInput(_) => BadRequest(e)
      case MissingIdentifier(_) => BadRequest(e)
      case RecordNotFound(_) => NotFound(e)
      case _ => InternalServerError(e)
    }

    implicit class StringOps(s: String) {

      import java.time.format.DateTimeFormatter

      lazy val localformatter: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")
      lazy val datetimeformatter: DateTimeFormatter =
      DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSZ")

      def toLocalDateTime: LocalDateTime = LocalDateTime.parse(s,localformatter)
      def toZonedDateTime: ZonedDateTime = ZonedDateTime.parse(s, datetimeformatter)

    }

        /**
        * 
        * @return An endpoint representing a Unit
        */
        private def createUser(da: DataAccessor): Endpoint[Unit] =
        post("user" :: jsonBody[User] :: header("api_key")) { (user: User, authParamapi_key: String) =>
          da.User_createUser(user, authParamapi_key) match {
            case Left(error) => checkError(error)
            case Right(data) => Ok(data)
          }
        } handle {
          case e: Exception => BadRequest(e)
        }

        /**
        * 
        * @return An endpoint representing a Unit
        */
        private def createUsersWithArrayInput(da: DataAccessor): Endpoint[Unit] =
        post("user" :: "createWithArray" :: jsonBody[Seq[User]] :: header("api_key")) { (user: Seq[User], authParamapi_key: String) =>
          da.User_createUsersWithArrayInput(user, authParamapi_key) match {
            case Left(error) => checkError(error)
            case Right(data) => Ok(data)
          }
        } handle {
          case e: Exception => BadRequest(e)
        }

        /**
        * 
        * @return An endpoint representing a Unit
        */
        private def createUsersWithListInput(da: DataAccessor): Endpoint[Unit] =
        post("user" :: "createWithList" :: jsonBody[Seq[User]] :: header("api_key")) { (user: Seq[User], authParamapi_key: String) =>
          da.User_createUsersWithListInput(user, authParamapi_key) match {
            case Left(error) => checkError(error)
            case Right(data) => Ok(data)
          }
        } handle {
          case e: Exception => BadRequest(e)
        }

        /**
        * 
        * @return An endpoint representing a Unit
        */
        private def deleteUser(da: DataAccessor): Endpoint[Unit] =
        delete("user" :: string :: header("api_key")) { (username: String, authParamapi_key: String) =>
          da.User_deleteUser(username, authParamapi_key) match {
            case Left(error) => checkError(error)
            case Right(data) => Ok(data)
          }
        } handle {
          case e: Exception => BadRequest(e)
        }

        /**
        * 
        * @return An endpoint representing a User
        */
        private def getUserByName(da: DataAccessor): Endpoint[User] =
        get("user" :: string) { (username: String) =>
          da.User_getUserByName(username) match {
            case Left(error) => checkError(error)
            case Right(data) => Ok(data)
          }
        } handle {
          case e: Exception => BadRequest(e)
        }

        /**
        * 
        * @return An endpoint representing a String
        */
        private def loginUser(da: DataAccessor): Endpoint[String] =
        get("user" :: "login" :: param("username") :: param("password")) { (username: String, password: String) =>
          da.User_loginUser(username, password) match {
            case Left(error) => checkError(error)
            case Right(data) => Ok(data)
          }
        } handle {
          case e: Exception => BadRequest(e)
        }

        /**
        * 
        * @return An endpoint representing a Unit
        */
        private def logoutUser(da: DataAccessor): Endpoint[Unit] =
        get("user" :: "logout" :: header("api_key")) { (authParamapi_key: String) =>
          da.User_logoutUser(authParamapi_key) match {
            case Left(error) => checkError(error)
            case Right(data) => Ok(data)
          }
        } handle {
          case e: Exception => BadRequest(e)
        }

        /**
        * 
        * @return An endpoint representing a Unit
        */
        private def updateUser(da: DataAccessor): Endpoint[Unit] =
        put("user" :: string :: jsonBody[User] :: header("api_key")) { (username: String, user: User, authParamapi_key: String) =>
          da.User_updateUser(username, user, authParamapi_key) match {
            case Left(error) => checkError(error)
            case Right(data) => Ok(data)
          }
        } handle {
          case e: Exception => BadRequest(e)
        }


    implicit private def fileUploadToFile(fileUpload: FileUpload) : File = {
      fileUpload match {
        case upload: InMemoryFileUpload =>
          bytesToFile(Buf.ByteArray.Owned.extract(upload.content))
        case upload: OnDiskFileUpload =>
          upload.content
        case _ => null
      }
    }

    private def bytesToFile(input: Array[Byte]): java.io.File = {
      val file = Files.createTempFile("tmpUserApi", null).toFile
      val output = new FileOutputStream(file)
      output.write(input)
      file
    }

    // This assists in params(string) application (which must be Seq[A] in parameter list) when the param is used as a List[A] elsewhere.
    implicit def seqList[A](input: Seq[A]): List[A] = input.toList
}
