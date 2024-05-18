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

class TestEnumRefStringStubBuilder internal constructor(private val objectMapper: ObjectMapper, private val stub: MappingBuilder) {

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
            .apply {
                body?.let {
                    withHeader("Content-Type", "application/json")
                    withBody(objectMapper.writeValueAsString(it))
                }
            }
            .configurer()
    )
}

class TestQueryDatetimeDateStringStubBuilder internal constructor(private val objectMapper: ObjectMapper, private val stub: MappingBuilder) {

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
            .apply {
                body?.let {
                    withHeader("Content-Type", "application/json")
                    withBody(objectMapper.writeValueAsString(it))
                }
            }
            .configurer()
    )
}

class TestQueryIntegerBooleanStringStubBuilder internal constructor(private val objectMapper: ObjectMapper, private val stub: MappingBuilder) {

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
            .apply {
                body?.let {
                    withHeader("Content-Type", "application/json")
                    withBody(objectMapper.writeValueAsString(it))
                }
            }
            .configurer()
    )
}

class TestQueryStyleDeepObjectExplodeTrueObjectStubBuilder internal constructor(private val objectMapper: ObjectMapper, private val stub: MappingBuilder) {

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
            .apply {
                body?.let {
                    withHeader("Content-Type", "application/json")
                    withBody(objectMapper.writeValueAsString(it))
                }
            }
            .configurer()
    )
}

class TestQueryStyleFormExplodeTrueArrayStringStubBuilder internal constructor(private val objectMapper: ObjectMapper, private val stub: MappingBuilder) {

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
            .apply {
                body?.let {
                    withHeader("Content-Type", "application/json")
                    withBody(objectMapper.writeValueAsString(it))
                }
            }
            .configurer()
    )
}

class TestQueryStyleFormExplodeTrueObjectStubBuilder internal constructor(private val objectMapper: ObjectMapper, private val stub: MappingBuilder) {

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
            .apply {
                body?.let {
                    withHeader("Content-Type", "application/json")
                    withBody(objectMapper.writeValueAsString(it))
                }
            }
            .configurer()
    )
}

