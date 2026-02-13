package org.openapitools.api

import org.openapitools.model.ModelApiResponse
import org.springframework.data.domain.Pageable
import org.openapitools.model.Pet
import org.springframework.http.HttpStatus
import org.springframework.http.MediaType
import org.springframework.http.ResponseEntity
import org.springframework.web.context.request.NativeWebRequest
import kotlinx.coroutines.flow.Flow

import java.util.Optional

/**
 * A delegate to be called by the {@link PetApiController}}.
 * Implement this interface with a {@link org.springframework.stereotype.Service} annotated class.
 */
@javax.annotation.Generated(value = ["org.openapitools.codegen.languages.KotlinSpringServerCodegen"], comments = "Generator version: 7.20.0-SNAPSHOT")
interface PetApiDelegate {

    fun getRequest(): Optional<NativeWebRequest> = Optional.empty()

    /**
     * @see PetApi#addPet
     */
    suspend fun addPet(pet: Pet,
        exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Unit>


    /**
     * @see PetApi#deletePet
     */
    suspend fun deletePet(petId: kotlin.Long,
        apiKey: kotlin.String?,
        exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Unit>


    /**
     * @see PetApi#findPetsByStatus
     */
    fun findPetsByStatus(status: kotlin.collections.List<kotlin.String>,
        exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Flow<Pet>>


    /**
     * @see PetApi#findPetsByTags
     */
    fun findPetsByTags(tags: kotlin.collections.List<kotlin.String>,
        exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Flow<Pet>>


    /**
     * @see PetApi#getPetById
     */
    suspend fun getPetById(petId: kotlin.Long,
        exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Pet>


    /**
     * @see PetApi#listAllPetsPaginated
     */
    fun listAllPetsPaginated(exchange: org.springframework.web.server.ServerWebExchange,
        pageable: Pageable): ResponseEntity<Flow<Pet>>


    /**
     * @see PetApi#listPetsByIdPaginated
     */
    fun listPetsByIdPaginated(xRequestID: kotlin.String?,
        exchange: org.springframework.web.server.ServerWebExchange,
        pageable: Pageable): ResponseEntity<Flow<Pet>>


    /**
     * @see PetApi#listPetsByOwnerPaginated
     */
    fun listPetsByOwnerPaginated(ownerId: kotlin.Long,
        includeAdopted: kotlin.Boolean,
        exchange: org.springframework.web.server.ServerWebExchange,
        pageable: Pageable): ResponseEntity<Flow<Pet>>


    /**
     * @see PetApi#listPetsMixedParams
     */
    fun listPetsMixedParams(authorization: kotlin.String?,
        xTenantID: kotlin.String?,
        status: kotlin.String?,
        includeInactive: kotlin.Boolean?,
        exchange: org.springframework.web.server.ServerWebExchange,
        pageable: Pageable): ResponseEntity<Flow<Pet>>


    /**
     * @see PetApi#listPetsMultipleParams
     */
    fun listPetsMultipleParams(name: kotlin.String?,
        minAge: kotlin.Int?,
        maxAge: kotlin.Int?,
        tags: kotlin.collections.List<kotlin.String>?,
        exchange: org.springframework.web.server.ServerWebExchange,
        pageable: Pageable): ResponseEntity<Flow<Pet>>


    /**
     * @see PetApi#listPetsNoPagination
     */
    fun listPetsNoPagination(status: kotlin.String?,
        page: kotlin.Int?,
        size: kotlin.Int?,
        sort: kotlin.String?,
        exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Flow<Pet>>


    /**
     * @see PetApi#listPetsPartialPagination
     */
    fun listPetsPartialPagination(status: kotlin.String?,
        exchange: org.springframework.web.server.ServerWebExchange,
        pageable: Pageable): ResponseEntity<Flow<Pet>>


    /**
     * @see PetApi#listPetsWithFilterPaginated
     */
    fun listPetsWithFilterPaginated(status: kotlin.String?,
        name: kotlin.String?,
        exchange: org.springframework.web.server.ServerWebExchange,
        pageable: Pageable): ResponseEntity<Flow<Pet>>


    /**
     * @see PetApi#listPetsWithHeaderSize
     */
    fun listPetsWithHeaderSize(size: kotlin.String?,
        category: kotlin.String?,
        exchange: org.springframework.web.server.ServerWebExchange,
        pageable: Pageable): ResponseEntity<Flow<Pet>>


    /**
     * @see PetApi#updatePet
     */
    suspend fun updatePet(pet: Pet,
        exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Unit>


    /**
     * @see PetApi#updatePetWithForm
     */
    suspend fun updatePetWithForm(petId: kotlin.Long,
        name: kotlin.String?,
        status: kotlin.String?,
        exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Unit>


    /**
     * @see PetApi#uploadFile
     */
    suspend fun uploadFile(petId: kotlin.Long,
        additionalMetadata: kotlin.String?,
        file: org.springframework.web.multipart.MultipartFile,
        exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<ModelApiResponse>

}
