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

/**
 * WireMock stub request builder.
 */
open class PetApiStubs(private val objectMapper: ObjectMapper) {

    /**
     * Construct a stub for the operation addPet.
     *
     * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
     * @return A [AddPetStubBuilder] to configure the response, and the final [MappingBuilder].
     */
    fun addPet(configurer: MappingBuilder.() -> MappingBuilder = { this }): AddPetStubBuilder =
        AddPetStubBuilder(objectMapper, post(urlPathTemplate("/pet"))
            .configurer()
        )

    /**
     * Construct a stub for the operation deletePet.
     *
     * @param petId path parameter petId pattern.
     * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
     * @return A [DeletePetStubBuilder] to configure the response, and the final [MappingBuilder].
     */
    fun deletePet(petId: StringValuePattern, configurer: MappingBuilder.() -> MappingBuilder = { this }): DeletePetStubBuilder =
        DeletePetStubBuilder(objectMapper, delete(urlPathTemplate("/pet/{petId}"))
            .withPathParam("petId", petId)
            .configurer()
        )

    /**
     * Construct a stub for the operation findPetsByStatus.
     *
     * @param status query parameter status pattern.
     * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
     * @return A [FindPetsByStatusStubBuilder] to configure the response, and the final [MappingBuilder].
     */
    fun findPetsByStatus(status: StringValuePattern? = null, configurer: MappingBuilder.() -> MappingBuilder = { this }): FindPetsByStatusStubBuilder =
        FindPetsByStatusStubBuilder(objectMapper, get(urlPathTemplate("/pet/findByStatus"))
            .apply { status?.let { withQueryParam("status", it) } }
            .configurer()
        )

    /**
     * Construct a stub for the operation findPetsByTags.
     *
     * @param tags query parameter tags pattern.
     * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
     * @return A [FindPetsByTagsStubBuilder] to configure the response, and the final [MappingBuilder].
     */
    fun findPetsByTags(tags: StringValuePattern? = null, configurer: MappingBuilder.() -> MappingBuilder = { this }): FindPetsByTagsStubBuilder =
        FindPetsByTagsStubBuilder(objectMapper, get(urlPathTemplate("/pet/findByTags"))
            .apply { tags?.let { withQueryParam("tags", it) } }
            .configurer()
        )

    /**
     * Construct a stub for the operation getPetById.
     *
     * @param petId path parameter petId pattern.
     * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
     * @return A [GetPetByIdStubBuilder] to configure the response, and the final [MappingBuilder].
     */
    fun getPetById(petId: StringValuePattern, configurer: MappingBuilder.() -> MappingBuilder = { this }): GetPetByIdStubBuilder =
        GetPetByIdStubBuilder(objectMapper, get(urlPathTemplate("/pet/{petId}"))
            .withPathParam("petId", petId)
            .configurer()
        )

    /**
     * Construct a stub for the operation updatePet.
     *
     * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
     * @return A [UpdatePetStubBuilder] to configure the response, and the final [MappingBuilder].
     */
    fun updatePet(configurer: MappingBuilder.() -> MappingBuilder = { this }): UpdatePetStubBuilder =
        UpdatePetStubBuilder(objectMapper, put(urlPathTemplate("/pet"))
            .configurer()
        )

    /**
     * Construct a stub for the operation updatePetWithForm.
     *
     * @param petId path parameter petId pattern.
     * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
     * @return A [UpdatePetWithFormStubBuilder] to configure the response, and the final [MappingBuilder].
     */
    fun updatePetWithForm(petId: StringValuePattern, configurer: MappingBuilder.() -> MappingBuilder = { this }): UpdatePetWithFormStubBuilder =
        UpdatePetWithFormStubBuilder(objectMapper, post(urlPathTemplate("/pet/{petId}"))
            .withPathParam("petId", petId)
            .configurer()
        )

    /**
     * Construct a stub for the operation uploadFile.
     *
     * @param petId path parameter petId pattern.
     * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
     * @return A [UploadFileStubBuilder] to configure the response, and the final [MappingBuilder].
     */
    fun uploadFile(petId: StringValuePattern, configurer: MappingBuilder.() -> MappingBuilder = { this }): UploadFileStubBuilder =
        UploadFileStubBuilder(objectMapper, post(urlPathTemplate("/pet/{petId}/uploadImage"))
            .withPathParam("petId", petId)
            .configurer()
        )
}
