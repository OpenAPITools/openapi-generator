package api

import model.ApiResponse
import model.Pet
import play.api.libs.Files.TemporaryFile

@javax.annotation.Generated(value = Array("org.openapitools.codegen.languages.ScalaPlayFrameworkServerCodegen"), date = "2019-03-26T16:21:58.590+08:00[Asia/Hong_Kong]")
trait PetApi {
  /**
    * Add a new pet to the store
    * @param body Pet object that needs to be added to the store
    */
  def addPet(body: Pet): Unit

  /**
    * Deletes a pet
    * @param petId Pet id to delete
    */
  def deletePet(petId: Long, apiKey: Option[String]): Unit

  /**
    * Finds Pets by status
    * Multiple status values can be provided with comma separated strings
    * @param status Status values that need to be considered for filter
    */
  def findPetsByStatus(status: List[String]): List[Pet]

  /**
    * Finds Pets by tags
    * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
    * @param tags Tags to filter by
    */
  def findPetsByTags(tags: List[String]): List[Pet]

  /**
    * Find pet by ID
    * Returns a single pet
    * @param petId ID of pet to return
    */
  def getPetById(petId: Long): Pet

  /**
    * Update an existing pet
    * @param body Pet object that needs to be added to the store
    */
  def updatePet(body: Pet): Unit

  /**
    * Updates a pet in the store with form data
    * @param petId ID of pet that needs to be updated
    * @param name Updated name of the pet
    * @param status Updated status of the pet
    */
  def updatePetWithForm(petId: Long, name: Option[String], status: Option[String]): Unit

  /**
    * uploads an image
    * @param petId ID of pet to update
    * @param additionalMetadata Additional data to pass to server
    * @param file file to upload
    */
  def uploadFile(petId: Long, additionalMetadata: Option[String], file: Option[TemporaryFile]): ApiResponse
}
