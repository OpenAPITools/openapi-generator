package org.openapitools.api

import org.openapitools.model.ModelApiResponse
import org.openapitools.model.Pet
import org.springframework.http.ResponseEntity


interface PetApiService {

    fun addPet(body: Pet): ResponseEntity<Unit>

    fun deletePet(petId: kotlin.Long, apiKey: kotlin.String?): ResponseEntity<Unit>

    fun findPetsByStatus(status: kotlin.collections.List<kotlin.String>): ResponseEntity<List<Pet>>

    fun findPetsByTags(tags: kotlin.collections.List<kotlin.String>): ResponseEntity<List<Pet>>

    fun getPetById(petId: kotlin.Long): ResponseEntity<Pet>

    fun updatePet(body: Pet): ResponseEntity<Unit>

    fun updatePetWithForm(petId: kotlin.Long, name: kotlin.String?, status: kotlin.String?): ResponseEntity<Unit>

    fun uploadFile(petId: kotlin.Long, additionalMetadata: kotlin.String?, file: org.springframework.core.io.Resource?): ResponseEntity<ModelApiResponse>
}
