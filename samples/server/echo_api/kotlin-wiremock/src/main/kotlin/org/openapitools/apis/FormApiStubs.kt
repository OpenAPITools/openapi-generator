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
open class FormApiStubs(private val objectMapper: ObjectMapper) {

    /**
     * Construct a stub for the operation testFormIntegerBooleanString.
     *
     * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
     * @return A [TestFormIntegerBooleanStringStubBuilder] to configure the response, and the final [MappingBuilder].
     */
    fun testFormIntegerBooleanString(configurer: MappingBuilder.() -> MappingBuilder = { this }): TestFormIntegerBooleanStringStubBuilder =
        TestFormIntegerBooleanStringStubBuilder(objectMapper, post(urlPathTemplate("/form/integer/boolean/string"))
            .configurer()
        )

    /**
     * Construct a stub for the operation testFormOneof.
     *
     * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
     * @return A [TestFormOneofStubBuilder] to configure the response, and the final [MappingBuilder].
     */
    fun testFormOneof(configurer: MappingBuilder.() -> MappingBuilder = { this }): TestFormOneofStubBuilder =
        TestFormOneofStubBuilder(objectMapper, post(urlPathTemplate("/form/oneof"))
            .configurer()
        )
}
