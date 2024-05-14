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

open class PathApiStubs(protected val objectMapper: ObjectMapper) {

    fun testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath(pathString: StringValuePattern, pathInteger: StringValuePattern, enumNonrefStringPath: StringValuePattern, enumRefStringPath: StringValuePattern, configurer: MappingBuilder.() -> MappingBuilder = { this }): TestsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPathStubBuilder =
        TestsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPathStubBuilder(get("/path/string/{path_string}/integer/{path_integer}/{enum_nonref_string_path}/{enum_ref_string_path}")
            .withPathParam("pathString", pathString)
            .withPathParam("pathInteger", pathInteger)
            .withPathParam("enumNonrefStringPath", enumNonrefStringPath)
            .withPathParam("enumRefStringPath", enumRefStringPath)
            .configurer()
        )

    inner class TestsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPathStubBuilder(private val stub: MappingBuilder) {

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
