package org.openapitools.api

import org.openapitools.model.ModelApiResponse
import org.openapitools.model.Pet
import kotlinx.coroutines.flow.Flow;
import org.springframework.stereotype.Service
@Service
class PetApiServiceImpl : PetApiService {

    override suspend fun addPet(body: Pet): Unit {
        TODO("Implement me")
    }

    override suspend fun deletePet(petId: kotlin.Long, apiKey: kotlin.String?): Unit {
        TODO("Implement me")
    }

    override fun findPetsByStatus(status: kotlin.collections.List<kotlin.String>): Flow<Pet> {
        TODO("Implement me")
    }

    override fun findPetsByTags(tags: kotlin.collections.List<kotlin.String>): Flow<Pet> {
        TODO("Implement me")
    }

    override suspend fun getPetById(petId: kotlin.Long): Pet {
        TODO("Implement me")
    }

    override suspend fun updatePet(body: Pet): Unit {
        TODO("Implement me")
    }

    override suspend fun updatePetWithForm(petId: kotlin.Long, name: kotlin.String?, status: kotlin.String?): Unit {
        TODO("Implement me")
    }

    override suspend fun uploadFile(petId: kotlin.Long, additionalMetadata: kotlin.String?, file: org.springframework.core.io.Resource?): ModelApiResponse {
        TODO("Implement me")
    }
}
