package org.openapitools.server.apis

import org.openapitools.server.models.ModelApiResponse
import org.openapitools.server.models.Pet
import io.javalin.http.Context

class PetApiServiceImpl : PetApiService {

    override fun addPet(pet: Pet, ctx: Context): Pet {
        TODO("Implement me")
    }

    override fun deletePet(petId: kotlin.Long, apiKey: kotlin.String?, ctx: Context): Unit {
        TODO("Implement me")
    }

    override fun findPetsByStatus(status: kotlin.collections.List<kotlin.String>, ctx: Context): List<Pet> {
        TODO("Implement me")
    }

    override fun findPetsByTags(tags: kotlin.collections.List<kotlin.String>, ctx: Context): List<Pet> {
        TODO("Implement me")
    }

    override fun getPetById(petId: kotlin.Long, ctx: Context): Pet {
        TODO("Implement me")
    }

    override fun updatePet(pet: Pet, ctx: Context): Pet {
        TODO("Implement me")
    }

    override fun updatePetWithForm(petId: kotlin.Long, name: kotlin.String?, status: kotlin.String?, ctx: Context): Unit {
        TODO("Implement me")
    }

    override fun uploadFile(petId: kotlin.Long, additionalMetadata: kotlin.String?, file: io.javalin.http.UploadedFile?, ctx: Context): ModelApiResponse {
        TODO("Implement me")
    }
}
