package org.openapitools.apis

import java.io._
import org.openapitools._
import org.openapitools.models._
import org.openapitools.models.ApiResponse
import java.io.File
import org.openapitools.models.Pet
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

object PetApi {
    /**
    * Compiles all service endpoints.
    * @return Bundled compilation of all service endpoints.
    */
    def endpoints(da: DataAccessor) =
        addPet(da) :+:
        deletePet(da) :+:
        findPetsByStatus(da) :+:
        findPetsByTags(da) :+:
        getPetById(da) :+:
        updatePet(da) :+:
        updatePetWithForm(da) :+:
        uploadFile(da)


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
        * @return An endpoint representing a Pet
        */
        private def addPet(da: DataAccessor): Endpoint[Pet] =
        post("pet" :: jsonBody[Pet]) { (pet: Pet) =>
          da.Pet_addPet(pet) match {
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
        private def deletePet(da: DataAccessor): Endpoint[Unit] =
        delete("pet" :: long :: headerOption("api_key")) { (petId: Long, apiKey: Option[String]) =>
          da.Pet_deletePet(petId, apiKey) match {
            case Left(error) => checkError(error)
            case Right(data) => Ok(data)
          }
        } handle {
          case e: Exception => BadRequest(e)
        }

        /**
        * 
        * @return An endpoint representing a Seq[Pet]
        */
        private def findPetsByStatus(da: DataAccessor): Endpoint[Seq[Pet]] =
        get("pet" :: "findByStatus" :: params("status")) { (status: Seq[String]) =>
          da.Pet_findPetsByStatus(status) match {
            case Left(error) => checkError(error)
            case Right(data) => Ok(data)
          }
        } handle {
          case e: Exception => BadRequest(e)
        }

        /**
        * 
        * @return An endpoint representing a Seq[Pet]
        */
        private def findPetsByTags(da: DataAccessor): Endpoint[Seq[Pet]] =
        get("pet" :: "findByTags" :: params("tags")) { (tags: Seq[String]) =>
          da.Pet_findPetsByTags(tags) match {
            case Left(error) => checkError(error)
            case Right(data) => Ok(data)
          }
        } handle {
          case e: Exception => BadRequest(e)
        }

        /**
        * 
        * @return An endpoint representing a Pet
        */
        private def getPetById(da: DataAccessor): Endpoint[Pet] =
        get("pet" :: long :: header("api_key")) { (petId: Long, authParamapi_key: String) =>
          da.Pet_getPetById(petId, authParamapi_key) match {
            case Left(error) => checkError(error)
            case Right(data) => Ok(data)
          }
        } handle {
          case e: Exception => BadRequest(e)
        }

        /**
        * 
        * @return An endpoint representing a Pet
        */
        private def updatePet(da: DataAccessor): Endpoint[Pet] =
        put("pet" :: jsonBody[Pet]) { (pet: Pet) =>
          da.Pet_updatePet(pet) match {
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
        private def updatePetWithForm(da: DataAccessor): Endpoint[Unit] =
        post("pet" :: long :: paramOption("name") :: paramOption("status")) { (petId: Long, name: Option[String], status: Option[String]) =>
          da.Pet_updatePetWithForm(petId, name, status) match {
            case Left(error) => checkError(error)
            case Right(data) => Ok(data)
          }
        } handle {
          case e: Exception => BadRequest(e)
        }

        /**
        * 
        * @return An endpoint representing a ApiResponse
        */
        private def uploadFile(da: DataAccessor): Endpoint[ApiResponse] =
        post("pet" :: long :: "uploadImage" :: paramOption("additionalMetadata") :: fileUpload("file")) { (petId: Long, additionalMetadata: Option[String], file: FileUpload) =>
          da.Pet_uploadFile(petId, additionalMetadata, file) match {
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
      val file = Files.createTempFile("tmpPetApi", null).toFile
      val output = new FileOutputStream(file)
      output.write(input)
      file
    }

    // This assists in params(string) application (which must be Seq[A] in parameter list) when the param is used as a List[A] elsewhere.
    implicit def seqList[A](input: Seq[A]): List[A] = input.toList
}
