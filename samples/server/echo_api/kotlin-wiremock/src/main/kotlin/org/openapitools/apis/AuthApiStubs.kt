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

open class AuthApiStubs(private val objectMapper: ObjectMapper) {

    fun testAuthHttpBasic(configurer: MappingBuilder.() -> MappingBuilder = { this }): TestAuthHttpBasicStubBuilder =
        TestAuthHttpBasicStubBuilder(objectMapper, post(urlPathTemplate("/auth/http/basic"))
            .configurer()
        )

    fun testAuthHttpBearer(configurer: MappingBuilder.() -> MappingBuilder = { this }): TestAuthHttpBearerStubBuilder =
        TestAuthHttpBearerStubBuilder(objectMapper, post(urlPathTemplate("/auth/http/bearer"))
            .configurer()
        )
}
