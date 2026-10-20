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
open class PathApiStubs(private val objectMapper: ObjectMapper) {

    /**
     * Construct a stub for the operation testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath.
     *
     * @param pathString path parameter pathString pattern.
     * @param pathInteger path parameter pathInteger pattern.
     * @param enumNonrefStringPath path parameter enumNonrefStringPath pattern.
     * @param enumRefStringPath path parameter enumRefStringPath pattern.
     * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
     * @return A [TestsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPathStubBuilder] to configure the response, and the final [MappingBuilder].
     */
    fun testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath(pathString: StringValuePattern, pathInteger: StringValuePattern, enumNonrefStringPath: StringValuePattern, enumRefStringPath: StringValuePattern, configurer: MappingBuilder.() -> MappingBuilder = { this }): TestsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPathStubBuilder =
        TestsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPathStubBuilder(objectMapper, get(urlPathTemplate("/path/string/{path_string}/integer/{path_integer}/{enum_nonref_string_path}/{enum_ref_string_path}"))
            .withPathParam("path_string", pathString)
            .withPathParam("path_integer", pathInteger)
            .withPathParam("enum_nonref_string_path", enumNonrefStringPath)
            .withPathParam("enum_ref_string_path", enumRefStringPath)
            .configurer()
        )
}
