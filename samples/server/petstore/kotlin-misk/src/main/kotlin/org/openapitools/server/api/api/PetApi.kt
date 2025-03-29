package org.openapitools.server.api.api

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

interface PetApi {

    fun addPet(@Valid @RequestBody pet: Pet): Pet

    fun deletePet(@PathParam("petId") petId: kotlin.Long, @RequestHeader(value = "api_key") apiKey: kotlin.String?)

    fun findPetsByStatus( @QueryParam(value = "status") status: kotlin.Array<kotlin.String>): kotlin.Array<Pet>

    fun findPetsByTags( @QueryParam(value = "tags") tags: kotlin.Array<kotlin.String>): kotlin.Array<Pet>

    fun getPetById(@PathParam("petId") petId: kotlin.Long): Pet

    fun updatePet(@Valid @RequestBody pet: Pet): Pet

    fun updatePetWithForm(@PathParam("petId") petId: kotlin.Long, @QueryParam(value = "name") name: kotlin.String? , @QueryParam(value = "status") status: kotlin.String? )

    fun uploadFile(@PathParam("petId") petId: kotlin.Long, @QueryParam(value = "additionalMetadata") additionalMetadata: kotlin.String? , @Valid file: HttpCall): ModelApiResponse
}
