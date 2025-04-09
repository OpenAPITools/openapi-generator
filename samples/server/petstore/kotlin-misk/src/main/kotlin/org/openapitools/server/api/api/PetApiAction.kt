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
import misk.web.Delete
import misk.web.Description
import misk.web.Get
import misk.web.HttpCall
import misk.web.Patch
import misk.web.PathParam
import misk.web.Post
import misk.web.Put
import misk.web.QueryParam
import misk.web.RequestBody
import misk.web.RequestContentType
import misk.web.RequestHeader
import misk.web.ResponseContentType
import misk.web.actions.WebAction
import misk.web.interceptors.LogRequestResponse
import misk.web.mediatype.MediaTypes
import org.openapitools.server.api.model.ModelApiResponse
import org.openapitools.server.api.model.Pet

/**
 * Generated file, please change PetApiImpl.
 */
@Singleton
class PetApiAction @Inject constructor(
    private val petApi: PetApi
) : WebAction, PetApi {

    @Post("/pet")
    @Description("Add a new pet to the store")
    @RequestContentType(MediaTypes.APPLICATION_JSON, MediaTypes.APPLICATION_XML)
    @ResponseContentType(MediaTypes.APPLICATION_XML, MediaTypes.APPLICATION_JSON)
    @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
    override fun addPet(@Valid @RequestBody pet: Pet): Pet {
        TODO()
    }

    @Delete("/pet/{petId}")
    @Description("Deletes a pet")
    @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
    override fun deletePet(@PathParam("petId") petId: kotlin.Long, @RequestHeader(value = "api_key") apiKey: kotlin.String?) {
        TODO()
    }

    @Get("/pet/findByStatus")
    @Description("Finds Pets by status")
    @ResponseContentType(MediaTypes.APPLICATION_XML, MediaTypes.APPLICATION_JSON)
    @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
    override fun findPetsByStatus( @QueryParam(value = "status") status: kotlin.Array<kotlin.String>): kotlin.Array<Pet> {
        TODO()
    }

    @Get("/pet/findByTags")
    @Description("Finds Pets by tags")
    @ResponseContentType(MediaTypes.APPLICATION_XML, MediaTypes.APPLICATION_JSON)
    @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
    override fun findPetsByTags( @QueryParam(value = "tags") tags: kotlin.Array<kotlin.String>): kotlin.Array<Pet> {
        TODO()
    }

    @Get("/pet/{petId}")
    @Description("Find pet by ID")
    @ResponseContentType(MediaTypes.APPLICATION_XML, MediaTypes.APPLICATION_JSON)
    @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
    override fun getPetById(@PathParam("petId") petId: kotlin.Long): Pet {
        TODO()
    }

    @Put("/pet")
    @Description("Update an existing pet")
    @RequestContentType(MediaTypes.APPLICATION_JSON, MediaTypes.APPLICATION_XML)
    @ResponseContentType(MediaTypes.APPLICATION_XML, MediaTypes.APPLICATION_JSON)
    @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
    override fun updatePet(@Valid @RequestBody pet: Pet): Pet {
        TODO()
    }

    @Post("/pet/{petId}")
    @Description("Updates a pet in the store with form data")
    @RequestContentType(MediaTypes.APPLICATION_FORM_URLENCODED)
    @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
    override fun updatePetWithForm(@PathParam("petId") petId: kotlin.Long, @QueryParam(value = "name") name: kotlin.String? , @QueryParam(value = "status") status: kotlin.String? ) {
        TODO()
    }

    @Post("/pet/{petId}/uploadImage")
    @Description("uploads an image")
    @RequestContentType(MediaTypes.FORM_DATA)
    @ResponseContentType(MediaTypes.APPLICATION_JSON)
    @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
    override fun uploadFile(@PathParam("petId") petId: kotlin.Long, @QueryParam(value = "additionalMetadata") additionalMetadata: kotlin.String? , @Valid file: HttpCall): ModelApiResponse {
        TODO()
    }
}
