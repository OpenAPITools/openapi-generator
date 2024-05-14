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

open class AuthApiStubs(protected val objectMapper: ObjectMapper) {

    fun testAuthHttpBasic(configurer: MappingBuilder.() -> MappingBuilder = { this }): TestAuthHttpBasicStubBuilder =
        TestAuthHttpBasicStubBuilder(post("/auth/http/basic")
            .configurer()
        )

    inner class TestAuthHttpBasicStubBuilder(private val stub: MappingBuilder) {

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

    fun testAuthHttpBearer(configurer: MappingBuilder.() -> MappingBuilder = { this }): TestAuthHttpBearerStubBuilder =
        TestAuthHttpBearerStubBuilder(post("/auth/http/bearer")
            .configurer()
        )

    inner class TestAuthHttpBearerStubBuilder(private val stub: MappingBuilder) {

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
