package org.openapitools.server.api.api

import org.openapitools.server.api.model.ModelApiResponse
import org.openapitools.server.api.model.Pet


import jakarta.inject.Inject
import jakarta.inject.Singleton

// TODO("Only import what we need")
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
        private val petApi: PetApi
    ) : PetApi, WebAction {

        @Post("/pet")
        @Description("Add a new pet to the store")
        @RequestContentType(MediaTypes.APPLICATION_JSON, MediaTypes.APPLICATION_XML)
        @ResponseContentType(MediaTypes.APPLICATION_XML, MediaTypes.APPLICATION_JSON)
        @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
        @Suppress("unused")
        override fun addPet(@RequestBody(required = false) pet: Pet) =
            petApi.addPet(pet)

        @Delete("/pet/{petId}")
        @Description("Deletes a pet")
        @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
        @Suppress("unused")
        override fun deletePet(@PathParam("petId") petId: kotlin.Long, @RequestHeaders headers: Headers) =
            petApi.deletePet(petId, apiKey)

        @Get("/pet/findByStatus")
        @Description("Finds Pets by status")
        @ResponseContentType(MediaTypes.APPLICATION_XML, MediaTypes.APPLICATION_JSON)
        @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
        @Suppress("unused")
        override fun findPetsByStatus(@QueryParam status: kotlin.Array<kotlin.String>) =
            petApi.findPetsByStatus(status)

        @Get("/pet/findByTags")
        @Description("Finds Pets by tags")
        @ResponseContentType(MediaTypes.APPLICATION_XML, MediaTypes.APPLICATION_JSON)
        @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
        @Suppress("unused")
        override fun findPetsByTags(@QueryParam tags: kotlin.Array<kotlin.String>) =
            petApi.findPetsByTags(tags)

        @Get("/pet/{petId}")
        @Description("Find pet by ID")
        @ResponseContentType(MediaTypes.APPLICATION_XML, MediaTypes.APPLICATION_JSON)
        @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
        @Suppress("unused")
        override fun getPetById(@PathParam("petId") petId: kotlin.Long) =
            petApi.getPetById(petId)

        @Put("/pet")
        @Description("Update an existing pet")
        @RequestContentType(MediaTypes.APPLICATION_JSON, MediaTypes.APPLICATION_XML)
        @ResponseContentType(MediaTypes.APPLICATION_XML, MediaTypes.APPLICATION_JSON)
        @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
        @Suppress("unused")
        override fun updatePet(@RequestBody(required = false) pet: Pet) =
            petApi.updatePet(pet)

        @Post("/pet/{petId}")
        @Description("Updates a pet in the store with form data")
        @RequestContentType(MediaTypes.APPLICATION_FORM_URLENCODED)
        @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
        @Suppress("unused")
        override fun updatePetWithForm(@PathParam("petId") petId: kotlin.Long,,) =
            petApi.updatePetWithForm(petId, name, status)

        @Post("/pet/{petId}/uploadImage")
        @Description("uploads an image")
        @RequestContentType()
        @ResponseContentType(MediaTypes.APPLICATION_JSON)
        @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
        @Suppress("unused")
        override fun uploadFile(@PathParam("petId") petId: kotlin.Long,,) =
            petApi.uploadFile(petId, additionalMetadata, file)
    }
