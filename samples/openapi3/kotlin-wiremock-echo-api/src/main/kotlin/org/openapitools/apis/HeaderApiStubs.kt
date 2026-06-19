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
open class HeaderApiStubs(private val objectMapper: ObjectMapper) {

    /**
     * Construct a stub for the operation testHeaderIntegerBooleanStringEnums.
     *
     * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
     * @return A [TestHeaderIntegerBooleanStringEnumsStubBuilder] to configure the response, and the final [MappingBuilder].
     */
    fun testHeaderIntegerBooleanStringEnums(configurer: MappingBuilder.() -> MappingBuilder = { this }): TestHeaderIntegerBooleanStringEnumsStubBuilder =
        TestHeaderIntegerBooleanStringEnumsStubBuilder(objectMapper, get(urlPathTemplate("/header/integer/boolean/string/enums"))
            .configurer()
        )
}
