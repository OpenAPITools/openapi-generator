package org.openapitools.api

import org.openapitools.model.ModelApiResponse
import org.openapitools.model.Pet
import kotlinx.coroutines.flow.Flow;
interface PetApiService {

	suspend fun addPet(pet: Pet): Pet

	suspend fun deletePet(petId: kotlin.Long, apiKey: kotlin.String?): Unit

	fun findPetsByStatus(status: kotlin.collections.List<kotlin.String>): Flow<Pet>

	fun findPetsByTags(tags: kotlin.collections.List<kotlin.String>): Flow<Pet>

	suspend fun getPetById(petId: kotlin.Long): Pet

	suspend fun updatePet(pet: Pet): Pet

	suspend fun updatePetWithForm(petId: kotlin.Long, name: kotlin.String?, status: kotlin.String?): Unit

	suspend fun uploadFile(petId: kotlin.Long, additionalMetadata: kotlin.String?, file: org.springframework.core.io.Resource?): ModelApiResponse
}
