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
 *  Builder for WireMock stubs of operation testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath.
 */
class TestsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPathStubBuilder internal constructor(private val objectMapper: ObjectMapper, private val stub: MappingBuilder) {

    /**
     * Let the stub for testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath respond with HTTP status code 200.
     *
     * @param body Response body for the [MappingBuilder].
     * @param configurer Configurer for the [MappingBuilder], allowing for arbitrary changes.
     * @return A [MappingBuilder] to be registered with a WireMock instance.
     */
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

    /**
     * Let the stub for testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath respond with HTTP status code [code].
     *
     * @param body Response body for the [MappingBuilder].
     * @param configurer Configurer for the [MappingBuilder], allowing for arbitrary changes.
     * @return A [MappingBuilder] to be registered with a WireMock instance.
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

