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

open class FormApiStubs(private val objectMapper: ObjectMapper) {

    fun testFormIntegerBooleanString(configurer: MappingBuilder.() -> MappingBuilder = { this }): TestFormIntegerBooleanStringStubBuilder =
        TestFormIntegerBooleanStringStubBuilder(objectMapper, post(urlPathTemplate("/form/integer/boolean/string"))
            .configurer()
        )

    fun testFormOneof(configurer: MappingBuilder.() -> MappingBuilder = { this }): TestFormOneofStubBuilder =
        TestFormOneofStubBuilder(objectMapper, post(urlPathTemplate("/form/oneof"))
            .configurer()
        )
}
