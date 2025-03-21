package org.openapitools.server.api.api

import org.openapitools.server.api.model.ModelApiResponse
import org.openapitools.server.api.model.Pet

import javax.validation.Valid
import javax.validation.constraints.DecimalMax
import javax.validation.constraints.DecimalMin
import javax.validation.constraints.Email
import javax.validation.constraints.Max
import javax.validation.constraints.Min
import javax.validation.constraints.NotNull
import javax.validation.constraints.Pattern
import javax.validation.constraints.Size

import jakarta.inject.Inject
import jakarta.inject.Singleton

import misk.web.Delete
import misk.web.Description
import misk.web.Get
import misk.web.HttpCall
import misk.web.PathParam
import misk.web.Post
import misk.web.Put
import misk.web.QueryParam
import misk.web.RequestBody
import misk.web.RequestContentType
import misk.web.RequestHeaders
import misk.web.ResponseContentType
import misk.web.actions.WebAction
import misk.web.interceptors.LogRequestResponse
import misk.web.mediatype.MediaTypes
import okhttp3.Headers

    @Singleton
    class PetApiController @Inject constructor(
        //private val petApi: PetApi
    ) : WebAction {

        @Post("/pet")
        @Description("Add a new pet to the store")
        @RequestContentType(MediaTypes.APPLICATION_JSON, MediaTypes.APPLICATION_XML)
        @ResponseContentType(MediaTypes.APPLICATION_XML, MediaTypes.APPLICATION_JSON)
        @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
        fun addPet(@Valid @RequestBody pet: Pet): Pet {
            TODO()
        }

        @Delete("/pet/{petId}")
        @Description("Deletes a pet")
        @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
        fun deletePet( @PathParam("petId") petId: kotlin.Long,apiKey: Headers) {
            TODO()
        }

        @Get("/pet/findByStatus")
        @Description("Finds Pets by status")
        @ResponseContentType(MediaTypes.APPLICATION_XML, MediaTypes.APPLICATION_JSON)
        @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
        fun findPetsByStatus( @QueryParam(value = "status") status: kotlin.Array<kotlin.String>): kotlin.Array<Pet> {
            TODO()
        }

        @Get("/pet/findByTags")
        @Description("Finds Pets by tags")
        @ResponseContentType(MediaTypes.APPLICATION_XML, MediaTypes.APPLICATION_JSON)
        @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
        fun findPetsByTags( @QueryParam(value = "tags") tags: kotlin.Array<kotlin.String>): kotlin.Array<Pet> {
            TODO()
        }

        @Get("/pet/{petId}")
        @Description("Find pet by ID")
        @ResponseContentType(MediaTypes.APPLICATION_XML, MediaTypes.APPLICATION_JSON)
        @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
        fun getPetById( @PathParam("petId") petId: kotlin.Long): Pet {
            TODO()
        }

        @Put("/pet")
        @Description("Update an existing pet")
        @RequestContentType(MediaTypes.APPLICATION_JSON, MediaTypes.APPLICATION_XML)
        @ResponseContentType(MediaTypes.APPLICATION_XML, MediaTypes.APPLICATION_JSON)
        @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
        fun updatePet(@Valid @RequestBody pet: Pet): Pet {
            TODO()
        }

        @Post("/pet/{petId}")
        @Description("Updates a pet in the store with form data")
        @RequestContentType(MediaTypes.APPLICATION_FORM_URLENCODED)
        @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
        fun updatePetWithForm( @PathParam("petId") petId: kotlin.Long,@Valid@QueryParam("name") name: kotlin.String?,@Valid@QueryParam("status") status: kotlin.String?) {
            TODO()
        }

        @Post("/pet/{petId}/uploadImage")
        @Description("uploads an image")
        @RequestContentType(MediaTypes.APPLICATION_OCTETSTREAM /* unknown -> multipart/form-data */ )
        @ResponseContentType(MediaTypes.APPLICATION_JSON)
        @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
        fun uploadFile( @PathParam("petId") petId: kotlin.Long,@Valid@QueryParam("additionalMetadata") additionalMetadata: kotlin.String?,): ModelApiResponse {
            TODO()
        }
    }
