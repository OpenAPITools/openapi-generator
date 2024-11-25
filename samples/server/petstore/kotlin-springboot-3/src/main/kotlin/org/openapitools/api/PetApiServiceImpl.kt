package org.openapitools.api

import org.openapitools.model.ModelApiResponse
import org.openapitools.model.Pet
import org.springframework.stereotype.Service
@Service
class PetApiServiceImpl : PetApiService {

    override fun addPet(pet: Pet): Pet {
        TODO("Implement me")
    }

    override fun deletePet(petId: kotlin.Long, apiKey: kotlin.String?): Unit {
        TODO("Implement me")
    }

    override fun findPetsByStatus(status: kotlin.collections.List<kotlin.String>): List<Pet> {
        TODO("Implement me")
    }

    override fun findPetsByTags(tags: kotlin.collections.List<kotlin.String>): List<Pet> {
        TODO("Implement me")
    }

    override fun getPetById(petId: kotlin.Long): Pet {
        TODO("Implement me")
    }

    override fun updatePet(pet: Pet): Pet {
        TODO("Implement me")
    }

    override fun updatePetWithForm(petId: kotlin.Long, name: kotlin.String?, status: kotlin.String?): Unit {
        TODO("Implement me")
    }

    override fun uploadFile(petId: kotlin.Long, additionalMetadata: kotlin.String?, file: org.springframework.core.io.Resource?): ModelApiResponse {
        TODO("Implement me")
    }
}
