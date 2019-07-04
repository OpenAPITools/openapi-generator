package org.openapitools.api

import org.openapitools.model.ModelApiResponse
import org.openapitools.model.Pet
interface PetApiService {

	fun addPet(body: Pet): Unit

	fun deletePet(petId: kotlin.Long, apiKey: kotlin.String?): Unit

	fun findPetsByStatus(status: kotlin.collections.List<kotlin.String>): List<Pet>

	fun findPetsByTags(tags: kotlin.collections.List<kotlin.String>): List<Pet>

	fun getPetById(petId: kotlin.Long): Pet

	fun updatePet(body: Pet): Unit

	fun updatePetWithForm(petId: kotlin.Long, name: kotlin.String?, status: kotlin.String?): Unit

	fun uploadFile(petId: kotlin.Long, additionalMetadata: kotlin.String?, file: org.springframework.core.io.Resource?): ModelApiResponse
}
