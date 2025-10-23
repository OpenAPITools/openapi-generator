package org.openapitools.api

import org.openapitools.model.ModelApiResponse
import org.openapitools.model.Pet
import org.springframework.http.HttpStatus
import org.springframework.http.MediaType
import org.springframework.http.ResponseEntity
import org.springframework.web.context.request.NativeWebRequest

import java.util.Optional

/**
 * A delegate to be called by the {@link PetApiController}}.
 * Implement this interface with a {@link org.springframework.stereotype.Service} annotated class.
 */
@jakarta.annotation.Generated(value = ["org.openapitools.codegen.languages.KotlinSpringServerCodegen"], comments = "Generator version: 7.17.0-SNAPSHOT")
interface PetApiDelegate {

    fun getRequest(): Optional<NativeWebRequest> = Optional.empty()

    /**
     * @see PetApi#addPet
     */
    fun addPet(pet: Pet): ResponseEntity<Pet>


    /**
     * @see PetApi#deletePet
     */
    fun deletePet(petId: kotlin.Long,
        apiKey: kotlin.String?): ResponseEntity<Unit>


    /**
     * @see PetApi#findPetsByStatus
     */
    fun findPetsByStatus(status: kotlin.collections.List<kotlin.String>): ResponseEntity<List<Pet>>


    /**
     * @see PetApi#findPetsByTags
     */
    fun findPetsByTags(tags: kotlin.collections.List<kotlin.String>): ResponseEntity<List<Pet>>


    /**
     * @see PetApi#getPetById
     */
    fun getPetById(petId: kotlin.Long): ResponseEntity<Pet>


    /**
     * @see PetApi#updatePet
     */
    fun updatePet(pet: Pet): ResponseEntity<Pet>


    /**
     * @see PetApi#updatePetWithForm
     */
    fun updatePetWithForm(petId: kotlin.Long,
        name: kotlin.String?,
        status: kotlin.String?): ResponseEntity<Unit>


    /**
     * @see PetApi#uploadFile
     */
    fun uploadFile(petId: kotlin.Long,
        additionalMetadata: kotlin.String?,
        file: org.springframework.web.multipart.MultipartFile?): ResponseEntity<ModelApiResponse>

}
