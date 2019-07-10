package org.openapitools.api

import org.openapitools.model.ModelApiResponse
import org.openapitools.model.Pet
import kotlinx.coroutines.flow.Flow;
import org.springframework.stereotype.Service
@Service
class PetApiServiceImpl : PetApiService {

    override suspend fun addPet(pet: Pet): Unit {
        TODO("Implement me")
    }

    override suspend fun deletePet(petId: Long, apiKey: String?): Unit {
        TODO("Implement me")
    }

    override fun findPetsByStatus(status: List<String>): Flow<Pet> {
        TODO("Implement me")
    }

    override fun findPetsByTags(tags: List<String>, maxCount: Int?): Flow<Pet> {
        TODO("Implement me")
    }

    override suspend fun getPetById(petId: Long): Pet {
        TODO("Implement me")
    }

    override suspend fun updatePet(pet: Pet): Unit {
        TODO("Implement me")
    }

    override suspend fun updatePetWithForm(petId: Long, name: String?, status: String?): Unit {
        TODO("Implement me")
    }

    override suspend fun uploadFile(petId: Long, additionalMetadata: String?, file: org.springframework.core.io.Resource?): ModelApiResponse {
        TODO("Implement me")
    }
}
