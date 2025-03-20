package org.openapitools.server.apis

import org.openapitools.server.models.ModelApiResponse
import org.openapitools.server.models.Pet

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

    override fun uploadFile(petId: kotlin.Long, additionalMetadata: kotlin.String?, file: io.javalin.http.UploadedFile?): ModelApiResponse {
        TODO("Implement me")
    }
}
