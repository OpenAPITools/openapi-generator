package org.openapitools.server.apis

import org.openapitools.server.models.ModelApiResponse
import org.openapitools.server.models.Pet

class PetApiServiceImpl : PetApiService {

    override fun addPet(pet: Pet): Pet {
        TODO("Implement me")
    }

    override fun deletePet(petId: Long, apiKey: String?): Unit {
        TODO("Implement me")
    }

    override fun findPetsByStatus(status: List<String>): List<Pet> {
        TODO("Implement me")
    }

    override fun findPetsByTags(tags: List<String>): List<Pet> {
        TODO("Implement me")
    }

    override fun getPetById(petId: Long): Pet {
        TODO("Implement me")
    }

    override fun updatePet(pet: Pet): Pet {
        TODO("Implement me")
    }

    override fun updatePetWithForm(petId: Long, name: String?, status: String?): Unit {
        TODO("Implement me")
    }

    override fun uploadFile(petId: Long, additionalMetadata: String?, file: io.javalin.http.UploadedFile?): ModelApiResponse {
        TODO("Implement me")
    }
}
