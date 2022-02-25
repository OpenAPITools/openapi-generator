package api

import org.openapitools.OpenApiExceptions
import javax.inject.{Inject, Singleton}
import play.api.libs.json._
import play.api.mvc._
import model.ApiResponse
import model.Pet
import play.api.libs.Files.TemporaryFile


@Singleton
class PetApiController @Inject()(cc: ControllerComponents, api: PetApi) extends AbstractController(cc) {
  /**
    * POST /v2/pet
    */
  def addPet(): Action[AnyContent] = Action { request =>
    def executeApi(): Pet = {
      val pet = request.body.asJson.map(_.as[Pet]).getOrElse {
        throw new OpenApiExceptions.MissingRequiredParameterException("body", "pet")
      }
      api.addPet(pet)
    }

    val result = executeApi()
    val json = Json.toJson(result)
    Ok(json)
  }

  /**
    * DELETE /v2/pet/:petId
    * @param petId Pet id to delete
    */
  def deletePet(petId: Long): Action[AnyContent] = Action { request =>
    def executeApi(): Unit = {
      val apiKey = request.headers.get("api_key")
        
      api.deletePet(petId, apiKey)
    }

    executeApi()
    Ok
  }

  /**
    * GET /v2/pet/findByStatus?status=[value]
    */
  def findPetsByStatus(): Action[AnyContent] = Action { request =>
    def executeApi(): List[Pet] = {
      val status = request.getQueryString("status")
        .map(values => splitCollectionParam(values, "csv"))
        .getOrElse {
          throw new OpenApiExceptions.MissingRequiredParameterException("status", "query string")
        }
      api.findPetsByStatus(status)
    }

    val result = executeApi()
    val json = Json.toJson(result)
    Ok(json)
  }

  /**
    * GET /v2/pet/findByTags?tags=[value]
    */
  def findPetsByTags(): Action[AnyContent] = Action { request =>
    def executeApi(): List[Pet] = {
      val tags = request.getQueryString("tags")
        .map(values => splitCollectionParam(values, "csv"))
        .getOrElse {
          throw new OpenApiExceptions.MissingRequiredParameterException("tags", "query string")
        }
      api.findPetsByTags(tags)
    }

    val result = executeApi()
    val json = Json.toJson(result)
    Ok(json)
  }

  /**
    * GET /v2/pet/:petId
    * @param petId ID of pet to return
    */
  def getPetById(petId: Long): Action[AnyContent] = Action { request =>
    def executeApi(): Pet = {
      api.getPetById(petId)
    }

    val result = executeApi()
    val json = Json.toJson(result)
    Ok(json)
  }

  /**
    * PUT /v2/pet
    */
  def updatePet(): Action[AnyContent] = Action { request =>
    def executeApi(): Pet = {
      val pet = request.body.asJson.map(_.as[Pet]).getOrElse {
        throw new OpenApiExceptions.MissingRequiredParameterException("body", "pet")
      }
      api.updatePet(pet)
    }

    val result = executeApi()
    val json = Json.toJson(result)
    Ok(json)
  }

  /**
    * POST /v2/pet/:petId
    * @param petId ID of pet that needs to be updated
    */
  def updatePetWithForm(petId: Long): Action[AnyContent] = Action { request =>
    def executeApi(): Unit = {
      val name = (request.body.asMultipartFormData.map(_.asFormUrlEncoded) orElse request.body.asFormUrlEncoded)
        .flatMap(_.get("name"))
        .flatMap(_.headOption)
        
      val status = (request.body.asMultipartFormData.map(_.asFormUrlEncoded) orElse request.body.asFormUrlEncoded)
        .flatMap(_.get("status"))
        .flatMap(_.headOption)
        
      api.updatePetWithForm(petId, name, status)
    }

    executeApi()
    Ok
  }

  /**
    * POST /v2/pet/:petId/uploadImage
    * @param petId ID of pet to update
    */
  def uploadFile(petId: Long): Action[AnyContent] = Action { request =>
    def executeApi(): ApiResponse = {
      val additionalMetadata = (request.body.asMultipartFormData.map(_.asFormUrlEncoded) orElse request.body.asFormUrlEncoded)
        .flatMap(_.get("additionalMetadata"))
        .flatMap(_.headOption)
        
      val file = request.body.asMultipartFormData.flatMap(_.file("file").map(_.ref: TemporaryFile))
        
      api.uploadFile(petId, additionalMetadata, file)
    }

    val result = executeApi()
    val json = Json.toJson(result)
    Ok(json)
  }

  private def splitCollectionParam(paramValues: String, collectionFormat: String): List[String] = {
    val splitBy =
      collectionFormat match {
        case "csv" => ",+"
        case "tsv" => "\t+"
        case "ssv" => " +"
        case "pipes" => "|+"
      }

    paramValues.split(splitBy).toList
  }
}
