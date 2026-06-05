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
open class EchoApiStubs(private val objectMapper: ObjectMapper) {

    /**
     * Construct a stub for the operation testsEchoStringEscapingParamName.
     *
     * @param dollarParamName path parameter dollarParamName pattern.
     * @param filterDollarType query parameter filterDollarType pattern.
     * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
     * @return A [TestsEchoStringEscapingParamNameStubBuilder] to configure the response, and the final [MappingBuilder].
     */
    fun testsEchoStringEscapingParamName(dollarParamName: StringValuePattern, filterDollarType: StringValuePattern? = null, configurer: MappingBuilder.() -> MappingBuilder = { this }): TestsEchoStringEscapingParamNameStubBuilder =
        TestsEchoStringEscapingParamNameStubBuilder(objectMapper, get(urlPathTemplate("/echo/string-escaping/{\$paramName}"))
            .withPathParam("\$paramName", dollarParamName)
            .apply { filterDollarType?.let { withQueryParam("filter\$Type", it) } }
            .configurer()
        )
}
