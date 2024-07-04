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
open class AuthApiStubs(private val objectMapper: ObjectMapper) {

    /**
     * Construct a stub for the operation testAuthHttpBasic.
     *
     * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
     * @return A [TestAuthHttpBasicStubBuilder] to configure the response, and the final [MappingBuilder].
     */
    fun testAuthHttpBasic(configurer: MappingBuilder.() -> MappingBuilder = { this }): TestAuthHttpBasicStubBuilder =
        TestAuthHttpBasicStubBuilder(objectMapper, post(urlPathTemplate("/auth/http/basic"))
            .configurer()
        )

    /**
     * Construct a stub for the operation testAuthHttpBearer.
     *
     * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
     * @return A [TestAuthHttpBearerStubBuilder] to configure the response, and the final [MappingBuilder].
     */
    fun testAuthHttpBearer(configurer: MappingBuilder.() -> MappingBuilder = { this }): TestAuthHttpBearerStubBuilder =
        TestAuthHttpBearerStubBuilder(objectMapper, post(urlPathTemplate("/auth/http/bearer"))
            .configurer()
        )
}
