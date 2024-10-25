package org.openapitools.server.apis

import org.openapitools.server.models.ModelApiResponse
import org.openapitools.server.models.Pet
import io.javalin.http.Context

class PetApiServiceImpl : PetApiService {

    override fun addPet(pet: Pet, ctx: Context): Pet {
        TODO("Implement me")
    }

    override fun deletePet(petId: Long, apiKey: String?, ctx: Context): Unit {
        TODO("Implement me")
    }

    override fun findPetsByStatus(status: List<String>, ctx: Context): List<Pet> {
        TODO("Implement me")
    }

    override fun findPetsByTags(tags: List<String>, ctx: Context): List<Pet> {
        TODO("Implement me")
    }

    override fun getPetById(petId: Long, ctx: Context): Pet {
        TODO("Implement me")
    }

    override fun updatePet(pet: Pet, ctx: Context): Pet {
        TODO("Implement me")
    }

    override fun updatePetWithForm(petId: Long, name: String?, status: String?, ctx: Context): Unit {
        TODO("Implement me")
    }

    override fun uploadFile(petId: Long, additionalMetadata: String?, file: io.javalin.http.UploadedFile?, ctx: Context): ModelApiResponse {
        TODO("Implement me")
    }
}
