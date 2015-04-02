package io.swagger.client.api

import io.swagger.client.model.Pet
import java.io.File
import io.swagger.client.core._
import io.swagger.client.core.CollectionFormats._
import io.swagger.client.core.ApiKeyLocations._

object PetApi {

  /**
   * 
   * Expected answers:
   *   code 405 :  (Validation exception)
   *   code 404 :  (Pet not found)
   *   code 400 :  (Invalid ID supplied)
   * 
   * @param Body Pet object that needs to be added to the store
   */
  def updatePet(Body: Option[Pet] = None): ApiRequest[Unit] =
    ApiRequest[Unit](ApiMethods.PUT, "http://petstore.swagger.io/v2", "/pet", "application/json")
      .withBody(Body)
      .withErrorResponse[Unit](405)
      .withErrorResponse[Unit](404)
      .withErrorResponse[Unit](400)
      
  /**
   * 
   * Expected answers:
   *   code 405 :  (Invalid input)
   * 
   * @param Body Pet object that needs to be added to the store
   */
  def addPet(Body: Option[Pet] = None): ApiRequest[Unit] =
    ApiRequest[Unit](ApiMethods.POST, "http://petstore.swagger.io/v2", "/pet", "application/json")
      .withBody(Body)
      .withErrorResponse[Unit](405)
      
  /**
   * Multiple status values can be provided with comma seperated strings
   * 
   * Expected answers:
   *   code 200 : Seq[Pet] (successful operation)
   *   code 400 :  (Invalid status value)
   * 
   * @param Status Status values that need to be considered for filter
   */
  def findPetsByStatus(Status: Seq[String]): ApiRequest[Seq[Pet]] =
    ApiRequest[Seq[Pet]](ApiMethods.GET, "http://petstore.swagger.io/v2", "/pet/findByStatus", "application/json")
      .withQueryParam("status", ArrayValues(Status, MULTI))
      .withSuccessResponse[Seq[Pet]](200)
      .withErrorResponse[Unit](400)
      
  /**
   * Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.
   * 
   * Expected answers:
   *   code 200 : Seq[Pet] (successful operation)
   *   code 400 :  (Invalid tag value)
   * 
   * @param Tags Tags to filter by
   */
  def findPetsByTags(Tags: Seq[String]): ApiRequest[Seq[Pet]] =
    ApiRequest[Seq[Pet]](ApiMethods.GET, "http://petstore.swagger.io/v2", "/pet/findByTags", "application/json")
      .withQueryParam("tags", ArrayValues(Tags, MULTI))
      .withSuccessResponse[Seq[Pet]](200)
      .withErrorResponse[Unit](400)
      
  /**
   * Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
   * 
   * Expected answers:
   *   code 404 :  (Pet not found)
   *   code 200 : Pet (successful operation)
   *   code 400 :  (Invalid ID supplied)
   * 
   * Available security schemes:
   *   api_key (apiKey)
   * 
   * @param PetId ID of pet that needs to be fetched
   */
  def getPetById(PetId: Long)(implicit apiKey: ApiKeyValue): ApiRequest[Pet] =
    ApiRequest[Pet](ApiMethods.GET, "http://petstore.swagger.io/v2", "/pet/{petId}", "application/json")
      .withApiKey(apiKey, "api_key", HEADER)
      .withPathParam("petId", PetId)
      .withErrorResponse[Unit](404)
      .withSuccessResponse[Pet](200)
      .withErrorResponse[Unit](400)
      
  /**
   * 
   * Expected answers:
   *   code 405 :  (Invalid input)
   * 
   * @param PetId ID of pet that needs to be updated
   * @param Name Updated name of the pet
   * @param Status Updated status of the pet
   */
  def updatePetWithForm(PetId: String, Name: Option[String] = None, Status: Option[String] = None): ApiRequest[Unit] =
    ApiRequest[Unit](ApiMethods.POST, "http://petstore.swagger.io/v2", "/pet/{petId}", "application/x-www-form-urlencoded")
      .withFormParam("name", Name)
      .withFormParam("status", Status)
      .withPathParam("petId", PetId)
      .withErrorResponse[Unit](405)
      
  /**
   * 
   * Expected answers:
   *   code 400 :  (Invalid pet value)
   * 
   * @param ApiKey 
   * @param PetId Pet id to delete
   */
  def deletePet(ApiKey: Option[String] = None, PetId: Long): ApiRequest[Unit] =
    ApiRequest[Unit](ApiMethods.DELETE, "http://petstore.swagger.io/v2", "/pet/{petId}", "application/json")
      .withPathParam("petId", PetId)
      .withHeaderParam("api_key", ApiKey)
      .withErrorResponse[Unit](400)
      
  /**
   * 
   * Expected answers:
   *   code 0 :  (successful operation)
   * 
   * @param PetId ID of pet to update
   * @param AdditionalMetadata Additional data to pass to server
   * @param File file to upload
   */
  def uploadFile(PetId: Long, AdditionalMetadata: Option[String] = None, File: Option[File] = None): ApiRequest[UnitUnit] =
    ApiRequest[UnitUnit](ApiMethods.POST, "http://petstore.swagger.io/v2", "/pet/{petId}/uploadImage", "multipart/form-data")
      .withFormParam("additionalMetadata", AdditionalMetadata)
      .withFormParam("file", File)
      .withPathParam("petId", PetId)
      .withSuccessResponse[Unit](0)
      


}

