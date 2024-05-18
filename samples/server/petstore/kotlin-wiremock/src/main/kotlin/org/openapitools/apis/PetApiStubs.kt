@file:Suppress(
    "RemoveRedundantQualifierName",
    "UnusedImport",
    "unused",
)

package org.openapitools.apis

import com.fasterxml.jackson.databind.ObjectMapper
import com.github.tomakehurst.wiremock.client.MappingBuilder
import com.github.tomakehurst.wiremock.client.ResponseDefinitionBuilder
import com.github.tomakehurst.wiremock.client.WireMock.*
import com.github.tomakehurst.wiremock.matching.StringValuePattern
import org.openapitools.models.*

open class PetApiStubs(private val objectMapper: ObjectMapper) {

    fun addPet(configurer: MappingBuilder.() -> MappingBuilder = { this }): AddPetStubBuilder =
        AddPetStubBuilder(objectMapper, post(urlPathTemplate("/pet"))
            .configurer()
        )

    fun deletePet(petId: StringValuePattern, configurer: MappingBuilder.() -> MappingBuilder = { this }): DeletePetStubBuilder =
        DeletePetStubBuilder(objectMapper, delete(urlPathTemplate("/pet/{petId}"))
            .withPathParam("petId", petId)
            .configurer()
        )

    fun findPetsByStatus(status: StringValuePattern? = null, configurer: MappingBuilder.() -> MappingBuilder = { this }): FindPetsByStatusStubBuilder =
        FindPetsByStatusStubBuilder(objectMapper, get(urlPathTemplate("/pet/findByStatus"))
            .apply { status?.let { withQueryParam("status", it) } }
            .configurer()
        )

    fun findPetsByTags(tags: StringValuePattern? = null, configurer: MappingBuilder.() -> MappingBuilder = { this }): FindPetsByTagsStubBuilder =
        FindPetsByTagsStubBuilder(objectMapper, get(urlPathTemplate("/pet/findByTags"))
            .apply { tags?.let { withQueryParam("tags", it) } }
            .configurer()
        )

    fun getPetById(petId: StringValuePattern, configurer: MappingBuilder.() -> MappingBuilder = { this }): GetPetByIdStubBuilder =
        GetPetByIdStubBuilder(objectMapper, get(urlPathTemplate("/pet/{petId}"))
            .withPathParam("petId", petId)
            .configurer()
        )

    fun updatePet(configurer: MappingBuilder.() -> MappingBuilder = { this }): UpdatePetStubBuilder =
        UpdatePetStubBuilder(objectMapper, put(urlPathTemplate("/pet"))
            .configurer()
        )

    fun updatePetWithForm(petId: StringValuePattern, configurer: MappingBuilder.() -> MappingBuilder = { this }): UpdatePetWithFormStubBuilder =
        UpdatePetWithFormStubBuilder(objectMapper, post(urlPathTemplate("/pet/{petId}"))
            .withPathParam("petId", petId)
            .configurer()
        )

    fun uploadFile(petId: StringValuePattern, configurer: MappingBuilder.() -> MappingBuilder = { this }): UploadFileStubBuilder =
        UploadFileStubBuilder(objectMapper, post(urlPathTemplate("/pet/{petId}/uploadImage"))
            .withPathParam("petId", petId)
            .configurer()
        )
}
