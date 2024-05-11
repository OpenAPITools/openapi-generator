package org.openapitools.apis

import com.fasterxml.jackson.databind.ObjectMapper
import com.github.tomakehurst.wiremock.client.MappingBuilder
import com.github.tomakehurst.wiremock.client.ResponseDefinitionBuilder
import com.github.tomakehurst.wiremock.client.WireMock.*
import com.github.tomakehurst.wiremock.matching.StringValuePattern
import org.openapitools.models.*

open class PetApiStubs(protected val objectMapper: ObjectMapper) {

    fun addPet(configurer: MappingBuilder.() -> MappingBuilder = { this }): AddPetStubBuilder =
        AddPetStubBuilder(post("/pet")
            .configurer()
        )

    inner class AddPetStubBuilder(private val stub: MappingBuilder) {

        fun respondWith200(
            body: Pet,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .configurer()
                .withStatus(200)
                .withHeader("Content-Type", "application/json")
                .withBody(objectMapper.writeValueAsString(body))
            )

        fun respondWith405(
            body: Pet,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .configurer()
                .withStatus(405)
                .withHeader("Content-Type", "application/json")
                .withBody(objectMapper.writeValueAsString(body))
            )

        fun respondWith(
            code: Int,
            body: Any? = null,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this }
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .configurer()
                .withStatus(code)
                .apply {
                    body?.let { withBody(objectMapper.writeValueAsString(it)) }
                }
        )
    }

    fun deletePet(petId: StringValuePattern, configurer: MappingBuilder.() -> MappingBuilder = { this }): DeletePetStubBuilder =
        DeletePetStubBuilder(delete("/pet/{petId}")
            .configurer()
            .withPathParam("petId", petId)
        )

    inner class DeletePetStubBuilder(private val stub: MappingBuilder) {

        fun respondWith400(
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .configurer()
                .withStatus(400)
            )

        fun respondWith(
            code: Int,
            body: Any? = null,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this }
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .configurer()
                .withStatus(code)
                .apply {
                    body?.let { withBody(objectMapper.writeValueAsString(it)) }
                }
        )
    }

    fun findPetsByStatus(status: StringValuePattern? = null, configurer: MappingBuilder.() -> MappingBuilder = { this }): FindPetsByStatusStubBuilder =
        FindPetsByStatusStubBuilder(get("/pet/findByStatus")
            .configurer()
            .apply {
                status?.let { withQueryParam("status", it) }
            }
        )

    inner class FindPetsByStatusStubBuilder(private val stub: MappingBuilder) {

        fun respondWith200(
            body: kotlin.collections.List<Pet>,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .configurer()
                .withStatus(200)
                .withHeader("Content-Type", "application/json")
                .withBody(objectMapper.writeValueAsString(body))
            )

        fun respondWith400(
            body: kotlin.collections.List<Pet>,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .configurer()
                .withStatus(400)
                .withHeader("Content-Type", "application/json")
                .withBody(objectMapper.writeValueAsString(body))
            )

        fun respondWith(
            code: Int,
            body: Any? = null,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this }
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .configurer()
                .withStatus(code)
                .apply {
                    body?.let { withBody(objectMapper.writeValueAsString(it)) }
                }
        )
    }

    fun findPetsByTags(tags: StringValuePattern? = null, configurer: MappingBuilder.() -> MappingBuilder = { this }): FindPetsByTagsStubBuilder =
        FindPetsByTagsStubBuilder(get("/pet/findByTags")
            .configurer()
            .apply {
                tags?.let { withQueryParam("tags", it) }
            }
        )

    inner class FindPetsByTagsStubBuilder(private val stub: MappingBuilder) {

        fun respondWith200(
            body: kotlin.collections.List<Pet>,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .configurer()
                .withStatus(200)
                .withHeader("Content-Type", "application/json")
                .withBody(objectMapper.writeValueAsString(body))
            )

        fun respondWith400(
            body: kotlin.collections.List<Pet>,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .configurer()
                .withStatus(400)
                .withHeader("Content-Type", "application/json")
                .withBody(objectMapper.writeValueAsString(body))
            )

        fun respondWith(
            code: Int,
            body: Any? = null,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this }
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .configurer()
                .withStatus(code)
                .apply {
                    body?.let { withBody(objectMapper.writeValueAsString(it)) }
                }
        )
    }

    fun getPetById(petId: StringValuePattern, configurer: MappingBuilder.() -> MappingBuilder = { this }): GetPetByIdStubBuilder =
        GetPetByIdStubBuilder(get("/pet/{petId}")
            .configurer()
            .withPathParam("petId", petId)
        )

    inner class GetPetByIdStubBuilder(private val stub: MappingBuilder) {

        fun respondWith200(
            body: Pet,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .configurer()
                .withStatus(200)
                .withHeader("Content-Type", "application/json")
                .withBody(objectMapper.writeValueAsString(body))
            )

        fun respondWith400(
            body: Pet,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .configurer()
                .withStatus(400)
                .withHeader("Content-Type", "application/json")
                .withBody(objectMapper.writeValueAsString(body))
            )

        fun respondWith404(
            body: Pet,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .configurer()
                .withStatus(404)
                .withHeader("Content-Type", "application/json")
                .withBody(objectMapper.writeValueAsString(body))
            )

        fun respondWith(
            code: Int,
            body: Any? = null,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this }
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .configurer()
                .withStatus(code)
                .apply {
                    body?.let { withBody(objectMapper.writeValueAsString(it)) }
                }
        )
    }

    fun updatePet(configurer: MappingBuilder.() -> MappingBuilder = { this }): UpdatePetStubBuilder =
        UpdatePetStubBuilder(put("/pet")
            .configurer()
        )

    inner class UpdatePetStubBuilder(private val stub: MappingBuilder) {

        fun respondWith200(
            body: Pet,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .configurer()
                .withStatus(200)
                .withHeader("Content-Type", "application/json")
                .withBody(objectMapper.writeValueAsString(body))
            )

        fun respondWith400(
            body: Pet,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .configurer()
                .withStatus(400)
                .withHeader("Content-Type", "application/json")
                .withBody(objectMapper.writeValueAsString(body))
            )

        fun respondWith404(
            body: Pet,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .configurer()
                .withStatus(404)
                .withHeader("Content-Type", "application/json")
                .withBody(objectMapper.writeValueAsString(body))
            )

        fun respondWith405(
            body: Pet,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .configurer()
                .withStatus(405)
                .withHeader("Content-Type", "application/json")
                .withBody(objectMapper.writeValueAsString(body))
            )

        fun respondWith(
            code: Int,
            body: Any? = null,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this }
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .configurer()
                .withStatus(code)
                .apply {
                    body?.let { withBody(objectMapper.writeValueAsString(it)) }
                }
        )
    }

    fun updatePetWithForm(petId: StringValuePattern, configurer: MappingBuilder.() -> MappingBuilder = { this }): UpdatePetWithFormStubBuilder =
        UpdatePetWithFormStubBuilder(post("/pet/{petId}")
            .configurer()
            .withPathParam("petId", petId)
        )

    inner class UpdatePetWithFormStubBuilder(private val stub: MappingBuilder) {

        fun respondWith405(
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .configurer()
                .withStatus(405)
            )

        fun respondWith(
            code: Int,
            body: Any? = null,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this }
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .configurer()
                .withStatus(code)
                .apply {
                    body?.let { withBody(objectMapper.writeValueAsString(it)) }
                }
        )
    }

    fun uploadFile(petId: StringValuePattern, configurer: MappingBuilder.() -> MappingBuilder = { this }): UploadFileStubBuilder =
        UploadFileStubBuilder(post("/pet/{petId}/uploadImage")
            .configurer()
            .withPathParam("petId", petId)
        )

    inner class UploadFileStubBuilder(private val stub: MappingBuilder) {

        fun respondWith200(
            body: ModelApiResponse,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .configurer()
                .withStatus(200)
                .withHeader("Content-Type", "application/json")
                .withBody(objectMapper.writeValueAsString(body))
            )

        fun respondWith(
            code: Int,
            body: Any? = null,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this }
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .configurer()
                .withStatus(code)
                .apply {
                    body?.let { withBody(objectMapper.writeValueAsString(it)) }
                }
        )
    }
}
