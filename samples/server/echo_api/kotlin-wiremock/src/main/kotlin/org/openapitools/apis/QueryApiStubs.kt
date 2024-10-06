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
open class QueryApiStubs(private val objectMapper: ObjectMapper) {

    /**
     * Construct a stub for the operation testEnumRefString.
     *
     * @param enumNonrefStringQuery query parameter enumNonrefStringQuery pattern.
     * @param enumRefStringQuery query parameter enumRefStringQuery pattern.
     * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
     * @return A [TestEnumRefStringStubBuilder] to configure the response, and the final [MappingBuilder].
     */
    fun testEnumRefString(enumNonrefStringQuery: StringValuePattern? = null, enumRefStringQuery: StringValuePattern? = null, configurer: MappingBuilder.() -> MappingBuilder = { this }): TestEnumRefStringStubBuilder =
        TestEnumRefStringStubBuilder(objectMapper, get(urlPathTemplate("/query/enum_ref_string"))
            .apply { enumNonrefStringQuery?.let { withQueryParam("enum_nonref_string_query", it) } }
            .apply { enumRefStringQuery?.let { withQueryParam("enum_ref_string_query", it) } }
            .configurer()
        )

    /**
     * Construct a stub for the operation testQueryDatetimeDateString.
     *
     * @param datetimeQuery query parameter datetimeQuery pattern.
     * @param dateQuery query parameter dateQuery pattern.
     * @param stringQuery query parameter stringQuery pattern.
     * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
     * @return A [TestQueryDatetimeDateStringStubBuilder] to configure the response, and the final [MappingBuilder].
     */
    fun testQueryDatetimeDateString(datetimeQuery: StringValuePattern? = null, dateQuery: StringValuePattern? = null, stringQuery: StringValuePattern? = null, configurer: MappingBuilder.() -> MappingBuilder = { this }): TestQueryDatetimeDateStringStubBuilder =
        TestQueryDatetimeDateStringStubBuilder(objectMapper, get(urlPathTemplate("/query/datetime/date/string"))
            .apply { datetimeQuery?.let { withQueryParam("datetime_query", it) } }
            .apply { dateQuery?.let { withQueryParam("date_query", it) } }
            .apply { stringQuery?.let { withQueryParam("string_query", it) } }
            .configurer()
        )

    /**
     * Construct a stub for the operation testQueryIntegerBooleanString.
     *
     * @param integerQuery query parameter integerQuery pattern.
     * @param booleanQuery query parameter booleanQuery pattern.
     * @param stringQuery query parameter stringQuery pattern.
     * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
     * @return A [TestQueryIntegerBooleanStringStubBuilder] to configure the response, and the final [MappingBuilder].
     */
    fun testQueryIntegerBooleanString(integerQuery: StringValuePattern? = null, booleanQuery: StringValuePattern? = null, stringQuery: StringValuePattern? = null, configurer: MappingBuilder.() -> MappingBuilder = { this }): TestQueryIntegerBooleanStringStubBuilder =
        TestQueryIntegerBooleanStringStubBuilder(objectMapper, get(urlPathTemplate("/query/integer/boolean/string"))
            .apply { integerQuery?.let { withQueryParam("integer_query", it) } }
            .apply { booleanQuery?.let { withQueryParam("boolean_query", it) } }
            .apply { stringQuery?.let { withQueryParam("string_query", it) } }
            .configurer()
        )

    /**
     * Construct a stub for the operation testQueryStyleDeepObjectExplodeTrueObject.
     *
     * @param queryObject query parameter queryObject pattern.
     * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
     * @return A [TestQueryStyleDeepObjectExplodeTrueObjectStubBuilder] to configure the response, and the final [MappingBuilder].
     */
    fun testQueryStyleDeepObjectExplodeTrueObject(queryObject: StringValuePattern? = null, configurer: MappingBuilder.() -> MappingBuilder = { this }): TestQueryStyleDeepObjectExplodeTrueObjectStubBuilder =
        TestQueryStyleDeepObjectExplodeTrueObjectStubBuilder(objectMapper, get(urlPathTemplate("/query/style_deepObject/explode_true/object"))
            .apply { queryObject?.let { withQueryParam("query_object", it) } }
            .configurer()
        )

    /**
     * Construct a stub for the operation testQueryStyleFormExplodeTrueArrayString.
     *
     * @param queryObject query parameter queryObject pattern.
     * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
     * @return A [TestQueryStyleFormExplodeTrueArrayStringStubBuilder] to configure the response, and the final [MappingBuilder].
     */
    fun testQueryStyleFormExplodeTrueArrayString(queryObject: StringValuePattern? = null, configurer: MappingBuilder.() -> MappingBuilder = { this }): TestQueryStyleFormExplodeTrueArrayStringStubBuilder =
        TestQueryStyleFormExplodeTrueArrayStringStubBuilder(objectMapper, get(urlPathTemplate("/query/style_form/explode_true/array_string"))
            .apply { queryObject?.let { withQueryParam("query_object", it) } }
            .configurer()
        )

    /**
     * Construct a stub for the operation testQueryStyleFormExplodeTrueObject.
     *
     * @param queryObject query parameter queryObject pattern.
     * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
     * @return A [TestQueryStyleFormExplodeTrueObjectStubBuilder] to configure the response, and the final [MappingBuilder].
     */
    fun testQueryStyleFormExplodeTrueObject(queryObject: StringValuePattern? = null, configurer: MappingBuilder.() -> MappingBuilder = { this }): TestQueryStyleFormExplodeTrueObjectStubBuilder =
        TestQueryStyleFormExplodeTrueObjectStubBuilder(objectMapper, get(urlPathTemplate("/query/style_form/explode_true/object"))
            .apply { queryObject?.let { withQueryParam("query_object", it) } }
            .configurer()
        )
}
