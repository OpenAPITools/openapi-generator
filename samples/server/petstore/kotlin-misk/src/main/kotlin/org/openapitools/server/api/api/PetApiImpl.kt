package org.openapitools.server.api.api

import org.openapitools.server.api.model.ModelApiResponse
import org.openapitools.server.api.model.Pet

import jakarta.inject.Inject
import jakarta.inject.Singleton
import okhttp3.Headers

/**
 * @TODO("Fill out implementation")
 */
@Singleton
class PetApiImpl @Inject constructor(
): PetApi {

    override fun addPet(pet: Pet): Pet {
        TODO()
    }

    override fun deletePet(petId: kotlin.Long, headers: Headers) {
        TODO()
    }

    override fun findPetsByStatus(status: kotlin.Array<kotlin.String>): kotlin.Array<Pet> {
        TODO()
    }

    override fun findPetsByTags(tags: kotlin.Array<kotlin.String>): kotlin.Array<Pet> {
        TODO()
    }

    override fun getPetById(petId: kotlin.Long): Pet {
        TODO()
    }

    override fun updatePet(pet: Pet): Pet {
        TODO()
    }

    override fun updatePetWithForm(petId: kotlin.Long,  name: kotlin.String?,  status: kotlin.String?) {
        TODO()
    }

    override fun uploadFile(petId: kotlin.Long,  additionalMetadata: kotlin.String?, ): ModelApiResponse {
        TODO()
    }
}
