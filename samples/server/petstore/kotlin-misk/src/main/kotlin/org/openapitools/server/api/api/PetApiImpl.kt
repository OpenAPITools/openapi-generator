package org.openapitools.server.api.api

import jakarta.inject.Inject
import jakarta.inject.Singleton
import jakarta.validation.Valid
import jakarta.validation.constraints.DecimalMax
import jakarta.validation.constraints.DecimalMin
import jakarta.validation.constraints.Email
import jakarta.validation.constraints.Max
import jakarta.validation.constraints.Min
import jakarta.validation.constraints.NotNull
import jakarta.validation.constraints.Pattern
import jakarta.validation.constraints.Size
import misk.web.HttpCall
import misk.web.PathParam
import misk.web.QueryParam
import misk.web.RequestBody
import misk.web.RequestHeader
import org.openapitools.server.api.model.ModelApiResponse
import org.openapitools.server.api.model.Pet

/**
 * @TODO("Fill out implementation")
 */
@Singleton
class PetApiImpl @Inject constructor(
): PetApi {

    override fun addPet(@Valid @RequestBody pet: Pet): Pet {
        TODO()
    }

    override fun deletePet(@PathParam("petId") petId: kotlin.Long, @RequestHeader(value = "api_key") apiKey: kotlin.String?) {
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
