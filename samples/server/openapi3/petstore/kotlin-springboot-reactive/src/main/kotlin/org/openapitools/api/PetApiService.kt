package org.openapitools.api

import org.openapitools.model.ModelApiResponse
import org.openapitools.model.Pet
import kotlinx.coroutines.flow.Flow;
interface PetApiService {

	suspend fun addPet(pet: Pet): Unit

	suspend fun deletePet(petId: Long, apiKey: String?): Unit

	fun findPetsByStatus(status: List<String>): Flow<Pet>

	fun findPetsByTags(tags: List<String>, maxCount: Int?): Flow<Pet>

	suspend fun getPetById(petId: Long): Pet

	suspend fun updatePet(pet: Pet): Unit

	suspend fun updatePetWithForm(petId: Long, name: String?, status: String?): Unit

	suspend fun uploadFile(petId: Long, additionalMetadata: String?, file: org.springframework.core.io.Resource?): ModelApiResponse
}
