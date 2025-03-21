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

    override fun addPet(@Valid @RequestBody pet: Pet): Pet {
        TODO()
    }

    override fun deletePet(@PathParam("petId") petId: kotlin.Long, apiKey: Headers) {
        TODO()
    }

    override fun findPetsByStatus( @QueryParam(value = "status") status: kotlin.Array<kotlin.String>): kotlin.Array<Pet> {
        TODO()
    }

    override fun findPetsByTags( @QueryParam(value = "tags") tags: kotlin.Array<kotlin.String>): kotlin.Array<Pet> {
        TODO()
    }

    override fun getPetById(@PathParam("petId") petId: kotlin.Long): Pet {
        TODO()
    }

    override fun updatePet(@Valid @RequestBody pet: Pet): Pet {
        TODO()
    }

    override fun updatePetWithForm(@PathParam("petId") petId: kotlin.Long, @QueryParam(value = "name") name: kotlin.String? , @QueryParam(value = "status") status: kotlin.String? ) {
        TODO()
    }

    override fun uploadFile(@PathParam("petId") petId: kotlin.Long, @QueryParam(value = "additionalMetadata") additionalMetadata: kotlin.String? , @Valid file: HttpCall): ModelApiResponse {
        TODO()
    }
}
