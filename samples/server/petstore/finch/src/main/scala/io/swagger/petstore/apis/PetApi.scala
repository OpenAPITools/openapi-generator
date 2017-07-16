package io.swagger.petstore.apis

import java.io._
import java.util.Date
import io.swagger.petstore._
import io.swagger.petstore.models._
import io.swagger.petstore.models.ApiResponse
import java.io.File
import io.swagger.petstore.models.Pet
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

        /**
        * 
        * @return And endpoint representing a Unit
        */
        private def addPet(da: DataAccessor): Endpoint[Unit] =
        post("pet"  :: jsonBody[Pet]) { (body: Pet) => 
                da.Pet_addPet(body)
                NoContent[Unit]
        } handle {
          case e: Exception => BadRequest(e)
        }

        /**
        * 
        * @return And endpoint representing a Unit
        */
        private def deletePet(da: DataAccessor): Endpoint[Unit] =
        delete("pet" :: long  :: string) { (petId: Long, apiKey: String) => 
                da.Pet_deletePet(petId, apiKey)
                NoContent[Unit]
        } handle {
          case e: Exception => BadRequest(e)
        }

        /**
        * 
        * @return And endpoint representing a Seq[Pet]
        */
        private def findPetsByStatus(da: DataAccessor): Endpoint[Seq[Pet]] =
        get("pet" :: "findByStatus"  :: params("status")) { (status: Seq[String]) => 
                Ok(da.Pet_findPetsByStatus(status))
        } handle {
          case e: Exception => BadRequest(e)
        }

        /**
        * 
        * @return And endpoint representing a Seq[Pet]
        */
        private def findPetsByTags(da: DataAccessor): Endpoint[Seq[Pet]] =
        get("pet" :: "findByTags"  :: params("tags")) { (tags: Seq[String]) => 
                Ok(da.Pet_findPetsByTags(tags))
        } handle {
          case e: Exception => BadRequest(e)
        }

        /**
        * 
        * @return And endpoint representing a Pet
        */
        private def getPetById(da: DataAccessor): Endpoint[Pet] =
        get("pet" :: long ) { (petId: Long) => 
                Ok(da.Pet_getPetById(petId))
        } handle {
          case e: Exception => BadRequest(e)
        }

        /**
        * 
        * @return And endpoint representing a Unit
        */
        private def updatePet(da: DataAccessor): Endpoint[Unit] =
        put("pet"  :: jsonBody[Pet]) { (body: Pet) => 
                da.Pet_updatePet(body)
                NoContent[Unit]
        } handle {
          case e: Exception => BadRequest(e)
        }

        /**
        * 
        * @return And endpoint representing a Unit
        */
        private def updatePetWithForm(da: DataAccessor): Endpoint[Unit] =
        post("pet" :: long  :: string :: string) { (petId: Long, name: String, status: String) => 
                da.Pet_updatePetWithForm(petId, name, status)
                NoContent[Unit]
        } handle {
          case e: Exception => BadRequest(e)
        }

        /**
        * 
        * @return And endpoint representing a ApiResponse
        */
        private def uploadFile(da: DataAccessor): Endpoint[ApiResponse] =
        post("pet" :: long :: "uploadImage"  :: string :: fileUpload("file")) { (petId: Long, additionalMetadata: String, file: FileUpload) => 
                Ok(da.Pet_uploadFile(petId, additionalMetadata, file))
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
        val file = File.createTempFile("tmpPetApi", null)
        val output = new FileOutputStream(file)
        output.write(input)
        file
    }

    // This assists in params(string) application (which must be Seq[A] in parameter list) when the param is used as a List[A] elsewhere.
    implicit def seqList[A](input: Seq[A]): List[A] = input.toList
}
