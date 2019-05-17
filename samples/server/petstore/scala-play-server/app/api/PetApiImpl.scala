package api

import model.ApiResponse
import model.Pet
import play.api.libs.Files.TemporaryFile

/**
  * Provides a default implementation for [[PetApi]].
  */
@javax.annotation.Generated(value = Array("org.openapitools.codegen.languages.ScalaPlayFrameworkServerCodegen"), date = "2019-03-26T16:21:58.590+08:00[Asia/Hong_Kong]")
class PetApiImpl extends PetApi {
  /**
    * @inheritdoc
    */
  override def addPet(body: Pet): Unit = {
    // TODO: Implement better logic

    
  }

  /**
    * @inheritdoc
    */
  override def deletePet(petId: Long, apiKey: Option[String]): Unit = {
    // TODO: Implement better logic

    
  }

  /**
    * @inheritdoc
    */
  override def findPetsByStatus(status: List[String]): List[Pet] = {
    // TODO: Implement better logic

    List.empty[Pet]
  }

  /**
    * @inheritdoc
    */
  override def findPetsByTags(tags: List[String]): List[Pet] = {
    // TODO: Implement better logic

    List.empty[Pet]
  }

  /**
    * @inheritdoc
    */
  override def getPetById(petId: Long): Pet = {
    // TODO: Implement better logic

    Pet(None, None, "", List.empty[String], None, None)
  }

  /**
    * @inheritdoc
    */
  override def updatePet(body: Pet): Unit = {
    // TODO: Implement better logic

    
  }

  /**
    * @inheritdoc
    */
  override def updatePetWithForm(petId: Long, name: Option[String], status: Option[String]): Unit = {
    // TODO: Implement better logic

    
  }

  /**
    * @inheritdoc
    */
  override def uploadFile(petId: Long, additionalMetadata: Option[String], file: Option[TemporaryFile]): ApiResponse = {
    // TODO: Implement better logic

    ApiResponse(None, None, None)
  }
}
