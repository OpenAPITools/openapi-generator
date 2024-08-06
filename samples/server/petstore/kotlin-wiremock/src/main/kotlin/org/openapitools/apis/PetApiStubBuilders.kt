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
 *  Builder for WireMock stubs of operation addPet.
 */
class AddPetStubBuilder internal constructor(private val objectMapper: ObjectMapper, private val stub: MappingBuilder) {

        /**
        * Let the stub for addPet respond with HTTP status code [code].
        *
        * @param code the response code.
        * @param body response body for the [MappingBuilder].
        * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
        * @return a [MappingBuilder] to be registered with a WireMock instance.
        */
        fun respondWith200(
            code: Int,
            body: Pet,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
        stub.willReturn(aResponse()
            .withStatus(code)
            .withHeader("Content-Type", "application/json")
            .withBody(objectMapper.writeValueAsString(body))
            .configurer()
        )

        /**
        * Let the stub for addPet respond with HTTP status code [code].
        *
        * @param code the response code.
        * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
        * @return a [MappingBuilder] to be registered with a WireMock instance.
        */
        fun respondWith405(
            code: Int,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
        stub.willReturn(aResponse()
            .withStatus(code)
            .configurer()
        )

    /**
     * Let the stub for addPet respond with HTTP status code [code].
     *
     * @param code the response code.
     * @param body response body for the [MappingBuilder].
     * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
     * @return a [MappingBuilder] to be registered with a WireMock instance.
     */
    fun respondWith(
        code: Int,
        body: Any? = null,
        configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this }
    ): MappingBuilder =
        stub.willReturn(aResponse()
            .withStatus(code)
            .apply {
                body?.let {
                    withHeader("Content-Type", "application/json")
                    withBody(objectMapper.writeValueAsString(it))
                }
            }
            .configurer()
    )
}

/**
 *  Builder for WireMock stubs of operation deletePet.
 */
class DeletePetStubBuilder internal constructor(private val objectMapper: ObjectMapper, private val stub: MappingBuilder) {

        /**
        * Let the stub for deletePet respond with HTTP status code [code].
        *
        * @param code the response code.
        * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
        * @return a [MappingBuilder] to be registered with a WireMock instance.
        */
        fun respondWith400(
            code: Int,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
        stub.willReturn(aResponse()
            .withStatus(code)
            .configurer()
        )

    /**
     * Let the stub for deletePet respond with HTTP status code [code].
     *
     * @param code the response code.
     * @param body response body for the [MappingBuilder].
     * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
     * @return a [MappingBuilder] to be registered with a WireMock instance.
     */
    fun respondWith(
        code: Int,
        body: Any? = null,
        configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this }
    ): MappingBuilder =
        stub.willReturn(aResponse()
            .withStatus(code)
            .apply {
                body?.let {
                    withHeader("Content-Type", "application/json")
                    withBody(objectMapper.writeValueAsString(it))
                }
            }
            .configurer()
    )
}

/**
 *  Builder for WireMock stubs of operation findPetsByStatus.
 */
class FindPetsByStatusStubBuilder internal constructor(private val objectMapper: ObjectMapper, private val stub: MappingBuilder) {

        /**
        * Let the stub for findPetsByStatus respond with HTTP status code [code].
        *
        * @param code the response code.
        * @param body response body for the [MappingBuilder].
        * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
        * @return a [MappingBuilder] to be registered with a WireMock instance.
        */
        fun respondWith200(
            code: Int,
            body: kotlin.collections.List<Pet>,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
        stub.willReturn(aResponse()
            .withStatus(code)
            .withHeader("Content-Type", "application/json")
            .withBody(objectMapper.writeValueAsString(body))
            .configurer()
        )

        /**
        * Let the stub for findPetsByStatus respond with HTTP status code [code].
        *
        * @param code the response code.
        * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
        * @return a [MappingBuilder] to be registered with a WireMock instance.
        */
        fun respondWith400(
            code: Int,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
        stub.willReturn(aResponse()
            .withStatus(code)
            .configurer()
        )

    /**
     * Let the stub for findPetsByStatus respond with HTTP status code [code].
     *
     * @param code the response code.
     * @param body response body for the [MappingBuilder].
     * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
     * @return a [MappingBuilder] to be registered with a WireMock instance.
     */
    fun respondWith(
        code: Int,
        body: Any? = null,
        configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this }
    ): MappingBuilder =
        stub.willReturn(aResponse()
            .withStatus(code)
            .apply {
                body?.let {
                    withHeader("Content-Type", "application/json")
                    withBody(objectMapper.writeValueAsString(it))
                }
            }
            .configurer()
    )
}

/**
 *  Builder for WireMock stubs of operation findPetsByTags.
 */
class FindPetsByTagsStubBuilder internal constructor(private val objectMapper: ObjectMapper, private val stub: MappingBuilder) {

        /**
        * Let the stub for findPetsByTags respond with HTTP status code [code].
        *
        * @param code the response code.
        * @param body response body for the [MappingBuilder].
        * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
        * @return a [MappingBuilder] to be registered with a WireMock instance.
        */
        fun respondWith200(
            code: Int,
            body: kotlin.collections.List<Pet>,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
        stub.willReturn(aResponse()
            .withStatus(code)
            .withHeader("Content-Type", "application/json")
            .withBody(objectMapper.writeValueAsString(body))
            .configurer()
        )

        /**
        * Let the stub for findPetsByTags respond with HTTP status code [code].
        *
        * @param code the response code.
        * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
        * @return a [MappingBuilder] to be registered with a WireMock instance.
        */
        fun respondWith400(
            code: Int,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
        stub.willReturn(aResponse()
            .withStatus(code)
            .configurer()
        )

    /**
     * Let the stub for findPetsByTags respond with HTTP status code [code].
     *
     * @param code the response code.
     * @param body response body for the [MappingBuilder].
     * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
     * @return a [MappingBuilder] to be registered with a WireMock instance.
     */
    fun respondWith(
        code: Int,
        body: Any? = null,
        configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this }
    ): MappingBuilder =
        stub.willReturn(aResponse()
            .withStatus(code)
            .apply {
                body?.let {
                    withHeader("Content-Type", "application/json")
                    withBody(objectMapper.writeValueAsString(it))
                }
            }
            .configurer()
    )
}

/**
 *  Builder for WireMock stubs of operation getPetById.
 */
class GetPetByIdStubBuilder internal constructor(private val objectMapper: ObjectMapper, private val stub: MappingBuilder) {

        /**
        * Let the stub for getPetById respond with HTTP status code [code].
        *
        * @param code the response code.
        * @param body response body for the [MappingBuilder].
        * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
        * @return a [MappingBuilder] to be registered with a WireMock instance.
        */
        fun respondWith200(
            code: Int,
            body: Pet,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
        stub.willReturn(aResponse()
            .withStatus(code)
            .withHeader("Content-Type", "application/json")
            .withBody(objectMapper.writeValueAsString(body))
            .configurer()
        )

        /**
        * Let the stub for getPetById respond with HTTP status code [code].
        *
        * @param code the response code.
        * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
        * @return a [MappingBuilder] to be registered with a WireMock instance.
        */
        fun respondWith400(
            code: Int,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
        stub.willReturn(aResponse()
            .withStatus(code)
            .configurer()
        )

        /**
        * Let the stub for getPetById respond with HTTP status code [code].
        *
        * @param code the response code.
        * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
        * @return a [MappingBuilder] to be registered with a WireMock instance.
        */
        fun respondWith404(
            code: Int,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
        stub.willReturn(aResponse()
            .withStatus(code)
            .configurer()
        )

    /**
     * Let the stub for getPetById respond with HTTP status code [code].
     *
     * @param code the response code.
     * @param body response body for the [MappingBuilder].
     * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
     * @return a [MappingBuilder] to be registered with a WireMock instance.
     */
    fun respondWith(
        code: Int,
        body: Any? = null,
        configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this }
    ): MappingBuilder =
        stub.willReturn(aResponse()
            .withStatus(code)
            .apply {
                body?.let {
                    withHeader("Content-Type", "application/json")
                    withBody(objectMapper.writeValueAsString(it))
                }
            }
            .configurer()
    )
}

/**
 *  Builder for WireMock stubs of operation updatePet.
 */
class UpdatePetStubBuilder internal constructor(private val objectMapper: ObjectMapper, private val stub: MappingBuilder) {

        /**
        * Let the stub for updatePet respond with HTTP status code [code].
        *
        * @param code the response code.
        * @param body response body for the [MappingBuilder].
        * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
        * @return a [MappingBuilder] to be registered with a WireMock instance.
        */
        fun respondWith200(
            code: Int,
            body: Pet,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
        stub.willReturn(aResponse()
            .withStatus(code)
            .withHeader("Content-Type", "application/json")
            .withBody(objectMapper.writeValueAsString(body))
            .configurer()
        )

        /**
        * Let the stub for updatePet respond with HTTP status code [code].
        *
        * @param code the response code.
        * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
        * @return a [MappingBuilder] to be registered with a WireMock instance.
        */
        fun respondWith400(
            code: Int,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
        stub.willReturn(aResponse()
            .withStatus(code)
            .configurer()
        )

        /**
        * Let the stub for updatePet respond with HTTP status code [code].
        *
        * @param code the response code.
        * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
        * @return a [MappingBuilder] to be registered with a WireMock instance.
        */
        fun respondWith404(
            code: Int,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
        stub.willReturn(aResponse()
            .withStatus(code)
            .configurer()
        )

        /**
        * Let the stub for updatePet respond with HTTP status code [code].
        *
        * @param code the response code.
        * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
        * @return a [MappingBuilder] to be registered with a WireMock instance.
        */
        fun respondWith405(
            code: Int,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
        stub.willReturn(aResponse()
            .withStatus(code)
            .configurer()
        )

    /**
     * Let the stub for updatePet respond with HTTP status code [code].
     *
     * @param code the response code.
     * @param body response body for the [MappingBuilder].
     * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
     * @return a [MappingBuilder] to be registered with a WireMock instance.
     */
    fun respondWith(
        code: Int,
        body: Any? = null,
        configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this }
    ): MappingBuilder =
        stub.willReturn(aResponse()
            .withStatus(code)
            .apply {
                body?.let {
                    withHeader("Content-Type", "application/json")
                    withBody(objectMapper.writeValueAsString(it))
                }
            }
            .configurer()
    )
}

/**
 *  Builder for WireMock stubs of operation updatePetWithForm.
 */
class UpdatePetWithFormStubBuilder internal constructor(private val objectMapper: ObjectMapper, private val stub: MappingBuilder) {

        /**
        * Let the stub for updatePetWithForm respond with HTTP status code [code].
        *
        * @param code the response code.
        * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
        * @return a [MappingBuilder] to be registered with a WireMock instance.
        */
        fun respondWith405(
            code: Int,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
        stub.willReturn(aResponse()
            .withStatus(code)
            .configurer()
        )

    /**
     * Let the stub for updatePetWithForm respond with HTTP status code [code].
     *
     * @param code the response code.
     * @param body response body for the [MappingBuilder].
     * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
     * @return a [MappingBuilder] to be registered with a WireMock instance.
     */
    fun respondWith(
        code: Int,
        body: Any? = null,
        configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this }
    ): MappingBuilder =
        stub.willReturn(aResponse()
            .withStatus(code)
            .apply {
                body?.let {
                    withHeader("Content-Type", "application/json")
                    withBody(objectMapper.writeValueAsString(it))
                }
            }
            .configurer()
    )
}

/**
 *  Builder for WireMock stubs of operation uploadFile.
 */
class UploadFileStubBuilder internal constructor(private val objectMapper: ObjectMapper, private val stub: MappingBuilder) {

        /**
        * Let the stub for uploadFile respond with HTTP status code [code].
        *
        * @param code the response code.
        * @param body response body for the [MappingBuilder].
        * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
        * @return a [MappingBuilder] to be registered with a WireMock instance.
        */
        fun respondWith200(
            code: Int,
            body: ApiResponse,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
        stub.willReturn(aResponse()
            .withStatus(code)
            .withHeader("Content-Type", "application/json")
            .withBody(objectMapper.writeValueAsString(body))
            .configurer()
        )

    /**
     * Let the stub for uploadFile respond with HTTP status code [code].
     *
     * @param code the response code.
     * @param body response body for the [MappingBuilder].
     * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
     * @return a [MappingBuilder] to be registered with a WireMock instance.
     */
    fun respondWith(
        code: Int,
        body: Any? = null,
        configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this }
    ): MappingBuilder =
        stub.willReturn(aResponse()
            .withStatus(code)
            .apply {
                body?.let {
                    withHeader("Content-Type", "application/json")
                    withBody(objectMapper.writeValueAsString(it))
                }
            }
            .configurer()
    )
}

