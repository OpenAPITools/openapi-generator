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

open class BodyApiStubs(protected val objectMapper: ObjectMapper) {

    fun testBinaryGif(configurer: MappingBuilder.() -> MappingBuilder = { this }): TestBinaryGifStubBuilder =
        TestBinaryGifStubBuilder(post("/binary/gif")
            .configurer()
        )

    inner class TestBinaryGifStubBuilder(private val stub: MappingBuilder) {

        fun respondWith200(
            body: java.io.File,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .withStatus(200)
                .withHeader("Content-Type", "application/json")
                .withBody(objectMapper.writeValueAsString(body))
                .configurer()
            )

        fun respondWith(
            code: Int,
            body: Any? = null,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this }
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .withStatus(code)
                .apply { body?.let { withBody(objectMapper.writeValueAsString(it)) } }
                .configurer()
        )
    }

    fun testBodyApplicationOctetstreamBinary(configurer: MappingBuilder.() -> MappingBuilder = { this }): TestBodyApplicationOctetstreamBinaryStubBuilder =
        TestBodyApplicationOctetstreamBinaryStubBuilder(post("/body/application/octetstream/binary")
            .configurer()
        )

    inner class TestBodyApplicationOctetstreamBinaryStubBuilder(private val stub: MappingBuilder) {

        fun respondWith200(
            body: kotlin.String,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .withStatus(200)
                .withHeader("Content-Type", "application/json")
                .withBody(objectMapper.writeValueAsString(body))
                .configurer()
            )

        fun respondWith(
            code: Int,
            body: Any? = null,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this }
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .withStatus(code)
                .apply { body?.let { withBody(objectMapper.writeValueAsString(it)) } }
                .configurer()
        )
    }

    fun testBodyMultipartFormdataArrayOfBinary(configurer: MappingBuilder.() -> MappingBuilder = { this }): TestBodyMultipartFormdataArrayOfBinaryStubBuilder =
        TestBodyMultipartFormdataArrayOfBinaryStubBuilder(post("/body/application/octetstream/array_of_binary")
            .configurer()
        )

    inner class TestBodyMultipartFormdataArrayOfBinaryStubBuilder(private val stub: MappingBuilder) {

        fun respondWith200(
            body: kotlin.String,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .withStatus(200)
                .withHeader("Content-Type", "application/json")
                .withBody(objectMapper.writeValueAsString(body))
                .configurer()
            )

        fun respondWith(
            code: Int,
            body: Any? = null,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this }
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .withStatus(code)
                .apply { body?.let { withBody(objectMapper.writeValueAsString(it)) } }
                .configurer()
        )
    }

    fun testBodyMultipartFormdataSingleBinary(configurer: MappingBuilder.() -> MappingBuilder = { this }): TestBodyMultipartFormdataSingleBinaryStubBuilder =
        TestBodyMultipartFormdataSingleBinaryStubBuilder(post("/body/application/octetstream/single_binary")
            .configurer()
        )

    inner class TestBodyMultipartFormdataSingleBinaryStubBuilder(private val stub: MappingBuilder) {

        fun respondWith200(
            body: kotlin.String,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .withStatus(200)
                .withHeader("Content-Type", "application/json")
                .withBody(objectMapper.writeValueAsString(body))
                .configurer()
            )

        fun respondWith(
            code: Int,
            body: Any? = null,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this }
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .withStatus(code)
                .apply { body?.let { withBody(objectMapper.writeValueAsString(it)) } }
                .configurer()
        )
    }

    fun testEchoBodyFreeFormObjectResponseString(configurer: MappingBuilder.() -> MappingBuilder = { this }): TestEchoBodyFreeFormObjectResponseStringStubBuilder =
        TestEchoBodyFreeFormObjectResponseStringStubBuilder(post("/echo/body/FreeFormObject/response_string")
            .configurer()
        )

    inner class TestEchoBodyFreeFormObjectResponseStringStubBuilder(private val stub: MappingBuilder) {

        fun respondWith200(
            body: kotlin.String,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .withStatus(200)
                .withHeader("Content-Type", "application/json")
                .withBody(objectMapper.writeValueAsString(body))
                .configurer()
            )

        fun respondWith(
            code: Int,
            body: Any? = null,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this }
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .withStatus(code)
                .apply { body?.let { withBody(objectMapper.writeValueAsString(it)) } }
                .configurer()
        )
    }

    fun testEchoBodyPet(configurer: MappingBuilder.() -> MappingBuilder = { this }): TestEchoBodyPetStubBuilder =
        TestEchoBodyPetStubBuilder(post("/echo/body/Pet")
            .configurer()
        )

    inner class TestEchoBodyPetStubBuilder(private val stub: MappingBuilder) {

        fun respondWith200(
            body: Pet,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .withStatus(200)
                .withHeader("Content-Type", "application/json")
                .withBody(objectMapper.writeValueAsString(body))
                .configurer()
            )

        fun respondWith(
            code: Int,
            body: Any? = null,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this }
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .withStatus(code)
                .apply { body?.let { withBody(objectMapper.writeValueAsString(it)) } }
                .configurer()
        )
    }

    fun testEchoBodyPetResponseString(configurer: MappingBuilder.() -> MappingBuilder = { this }): TestEchoBodyPetResponseStringStubBuilder =
        TestEchoBodyPetResponseStringStubBuilder(post("/echo/body/Pet/response_string")
            .configurer()
        )

    inner class TestEchoBodyPetResponseStringStubBuilder(private val stub: MappingBuilder) {

        fun respondWith200(
            body: kotlin.String,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .withStatus(200)
                .withHeader("Content-Type", "application/json")
                .withBody(objectMapper.writeValueAsString(body))
                .configurer()
            )

        fun respondWith(
            code: Int,
            body: Any? = null,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this }
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .withStatus(code)
                .apply { body?.let { withBody(objectMapper.writeValueAsString(it)) } }
                .configurer()
        )
    }

    fun testEchoBodyTagResponseString(configurer: MappingBuilder.() -> MappingBuilder = { this }): TestEchoBodyTagResponseStringStubBuilder =
        TestEchoBodyTagResponseStringStubBuilder(post("/echo/body/Tag/response_string")
            .configurer()
        )

    inner class TestEchoBodyTagResponseStringStubBuilder(private val stub: MappingBuilder) {

        fun respondWith200(
            body: kotlin.String,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this },
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .withStatus(200)
                .withHeader("Content-Type", "application/json")
                .withBody(objectMapper.writeValueAsString(body))
                .configurer()
            )

        fun respondWith(
            code: Int,
            body: Any? = null,
            configurer: ResponseDefinitionBuilder.() -> ResponseDefinitionBuilder = { this }
        ): MappingBuilder =
            stub.willReturn(aResponse()
                .withStatus(code)
                .apply { body?.let { withBody(objectMapper.writeValueAsString(it)) } }
                .configurer()
        )
    }
}
