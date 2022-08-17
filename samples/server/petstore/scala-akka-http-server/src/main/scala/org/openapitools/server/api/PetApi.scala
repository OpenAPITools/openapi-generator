package org.openapitools.server.api

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.marshalling.ToEntityMarshaller
import akka.http.scaladsl.unmarshalling.FromEntityUnmarshaller
import akka.http.scaladsl.unmarshalling.FromStringUnmarshaller
import org.openapitools.server.AkkaHttpHelper._
import org.openapitools.server.StringDirectives
import org.openapitools.server.MultipartDirectives
import org.openapitools.server.FileField
import org.openapitools.server.PartsAndFiles
import org.openapitools.server.model.ApiResponse
import java.io.File
import org.openapitools.server.model.Pet
import scala.util.Try
import akka.http.scaladsl.server.MalformedRequestContentRejection
import akka.http.scaladsl.server.directives.FileInfo


class PetApi(
    petService: PetApiService,
    petMarshaller: PetApiMarshaller
)  extends MultipartDirectives with StringDirectives {

  
  import petMarshaller._

  lazy val route: Route =
    path("pet") { 
      post {  
            entity(as[Pet]){ pet =>
              petService.addPet(pet = pet)
            }
      }
    } ~
    path("pet" / LongNumber) { (petId) => 
      delete {  
          optionalHeaderValueByName("api_key") { apiKey => 
            petService.deletePet(petId = petId, apiKey = apiKey)
          }
      }
    } ~
    path("pet" / "findByStatus") { 
      get { 
        parameters("status".as[String]) { (status) => 
            petService.findPetsByStatus(status = status)
        }
      }
    } ~
    path("pet" / "findByTags") { 
      get { 
        parameters("tags".as[String]) { (tags) => 
            petService.findPetsByTags(tags = tags)
        }
      }
    } ~
    path("pet" / LongNumber) { (petId) => 
      get {  
            petService.getPetById(petId = petId)
      }
    } ~
    path("pet") { 
      put {  
            entity(as[Pet]){ pet =>
              petService.updatePet(pet = pet)
            }
      }
    } ~
    path("pet" / LongNumber) { (petId) => 
      post {  
          formFields("name".as[String].?, "status".as[String].?) { (name, status) =>
            petService.updatePetWithForm(petId = petId, name = name, status = status)
          }
      }
    } ~
    path("pet" / LongNumber / "uploadImage") { (petId) => 
      post {  
        formAndFiles(FileField("file")) { partsAndFiles => 
          val _____ : Try[Route] = for {
            file <- optToTry(partsAndFiles.files.get("file"), s"File file missing")
          } yield { 
            implicit val vp: StringValueProvider = partsAndFiles.form
            stringFields("additionalMetadata".as[String].?) { (additionalMetadata) =>
              petService.uploadFile(petId = petId, additionalMetadata = additionalMetadata, file = file)
            }
          }
          _____.fold[Route](t => reject(MalformedRequestContentRejection("Missing file.", t)), identity)
        }
      }
    }
}


trait PetApiService {

  def addPet200(responsePet: Pet)(implicit toEntityMarshallerPet: ToEntityMarshaller[Pet]): Route =
    complete((200, responsePet))
  def addPet405: Route =
    complete((405, "Invalid input"))
  /**
   * Code: 200, Message: successful operation, DataType: Pet
   * Code: 405, Message: Invalid input
   */
  def addPet(pet: Pet)
      (implicit toEntityMarshallerPet: ToEntityMarshaller[Pet]): Route

  def deletePet400: Route =
    complete((400, "Invalid pet value"))
  /**
   * Code: 400, Message: Invalid pet value
   */
  def deletePet(petId: Long, apiKey: Option[String]): Route

  def findPetsByStatus200(responsePetarray: Seq[Pet])(implicit toEntityMarshallerPetarray: ToEntityMarshaller[Seq[Pet]]): Route =
    complete((200, responsePetarray))
  def findPetsByStatus400: Route =
    complete((400, "Invalid status value"))
  /**
   * Code: 200, Message: successful operation, DataType: Seq[Pet]
   * Code: 400, Message: Invalid status value
   */
  def findPetsByStatus(status: String)
      (implicit toEntityMarshallerPetarray: ToEntityMarshaller[Seq[Pet]]): Route

  def findPetsByTags200(responsePetarray: Seq[Pet])(implicit toEntityMarshallerPetarray: ToEntityMarshaller[Seq[Pet]]): Route =
    complete((200, responsePetarray))
  def findPetsByTags400: Route =
    complete((400, "Invalid tag value"))
  /**
   * Code: 200, Message: successful operation, DataType: Seq[Pet]
   * Code: 400, Message: Invalid tag value
   */
  def findPetsByTags(tags: String)
      (implicit toEntityMarshallerPetarray: ToEntityMarshaller[Seq[Pet]]): Route

  def getPetById200(responsePet: Pet)(implicit toEntityMarshallerPet: ToEntityMarshaller[Pet]): Route =
    complete((200, responsePet))
  def getPetById400: Route =
    complete((400, "Invalid ID supplied"))
  def getPetById404: Route =
    complete((404, "Pet not found"))
  /**
   * Code: 200, Message: successful operation, DataType: Pet
   * Code: 400, Message: Invalid ID supplied
   * Code: 404, Message: Pet not found
   */
  def getPetById(petId: Long)
      (implicit toEntityMarshallerPet: ToEntityMarshaller[Pet]): Route

  def updatePet200(responsePet: Pet)(implicit toEntityMarshallerPet: ToEntityMarshaller[Pet]): Route =
    complete((200, responsePet))
  def updatePet400: Route =
    complete((400, "Invalid ID supplied"))
  def updatePet404: Route =
    complete((404, "Pet not found"))
  def updatePet405: Route =
    complete((405, "Validation exception"))
  /**
   * Code: 200, Message: successful operation, DataType: Pet
   * Code: 400, Message: Invalid ID supplied
   * Code: 404, Message: Pet not found
   * Code: 405, Message: Validation exception
   */
  def updatePet(pet: Pet)
      (implicit toEntityMarshallerPet: ToEntityMarshaller[Pet]): Route

  def updatePetWithForm405: Route =
    complete((405, "Invalid input"))
  /**
   * Code: 405, Message: Invalid input
   */
  def updatePetWithForm(petId: Long, name: Option[String], status: Option[String]): Route

  def uploadFile200(responseApiResponse: ApiResponse)(implicit toEntityMarshallerApiResponse: ToEntityMarshaller[ApiResponse]): Route =
    complete((200, responseApiResponse))
  /**
   * Code: 200, Message: successful operation, DataType: ApiResponse
   */
  def uploadFile(petId: Long, additionalMetadata: Option[String], file: (FileInfo, File))
      (implicit toEntityMarshallerApiResponse: ToEntityMarshaller[ApiResponse]): Route

}

trait PetApiMarshaller {
  implicit def fromEntityUnmarshallerPet: FromEntityUnmarshaller[Pet]



  implicit def toEntityMarshallerPetarray: ToEntityMarshaller[Seq[Pet]]

  implicit def toEntityMarshallerPet: ToEntityMarshaller[Pet]

  implicit def toEntityMarshallerApiResponse: ToEntityMarshaller[ApiResponse]

}

