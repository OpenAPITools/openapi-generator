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

open class QueryApiStubs(private val objectMapper: ObjectMapper) {

    fun testEnumRefString(enumNonrefStringQuery: StringValuePattern? = null, enumRefStringQuery: StringValuePattern? = null, configurer: MappingBuilder.() -> MappingBuilder = { this }): TestEnumRefStringStubBuilder =
        TestEnumRefStringStubBuilder(objectMapper, get(urlPathTemplate("/query/enum_ref_string"))
            .apply { enumNonrefStringQuery?.let { withQueryParam("enumNonrefStringQuery", it) } }
            .apply { enumRefStringQuery?.let { withQueryParam("enumRefStringQuery", it) } }
            .configurer()
        )

    fun testQueryDatetimeDateString(datetimeQuery: StringValuePattern? = null, dateQuery: StringValuePattern? = null, stringQuery: StringValuePattern? = null, configurer: MappingBuilder.() -> MappingBuilder = { this }): TestQueryDatetimeDateStringStubBuilder =
        TestQueryDatetimeDateStringStubBuilder(objectMapper, get(urlPathTemplate("/query/datetime/date/string"))
            .apply { datetimeQuery?.let { withQueryParam("datetimeQuery", it) } }
            .apply { dateQuery?.let { withQueryParam("dateQuery", it) } }
            .apply { stringQuery?.let { withQueryParam("stringQuery", it) } }
            .configurer()
        )

    fun testQueryIntegerBooleanString(integerQuery: StringValuePattern? = null, booleanQuery: StringValuePattern? = null, stringQuery: StringValuePattern? = null, configurer: MappingBuilder.() -> MappingBuilder = { this }): TestQueryIntegerBooleanStringStubBuilder =
        TestQueryIntegerBooleanStringStubBuilder(objectMapper, get(urlPathTemplate("/query/integer/boolean/string"))
            .apply { integerQuery?.let { withQueryParam("integerQuery", it) } }
            .apply { booleanQuery?.let { withQueryParam("booleanQuery", it) } }
            .apply { stringQuery?.let { withQueryParam("stringQuery", it) } }
            .configurer()
        )

    fun testQueryStyleDeepObjectExplodeTrueObject(queryObject: StringValuePattern? = null, configurer: MappingBuilder.() -> MappingBuilder = { this }): TestQueryStyleDeepObjectExplodeTrueObjectStubBuilder =
        TestQueryStyleDeepObjectExplodeTrueObjectStubBuilder(objectMapper, get(urlPathTemplate("/query/style_deepObject/explode_true/object"))
            .apply { queryObject?.let { withQueryParam("queryObject", it) } }
            .configurer()
        )

    fun testQueryStyleFormExplodeTrueArrayString(queryObject: StringValuePattern? = null, configurer: MappingBuilder.() -> MappingBuilder = { this }): TestQueryStyleFormExplodeTrueArrayStringStubBuilder =
        TestQueryStyleFormExplodeTrueArrayStringStubBuilder(objectMapper, get(urlPathTemplate("/query/style_form/explode_true/array_string"))
            .apply { queryObject?.let { withQueryParam("queryObject", it) } }
            .configurer()
        )

    fun testQueryStyleFormExplodeTrueObject(queryObject: StringValuePattern? = null, configurer: MappingBuilder.() -> MappingBuilder = { this }): TestQueryStyleFormExplodeTrueObjectStubBuilder =
        TestQueryStyleFormExplodeTrueObjectStubBuilder(objectMapper, get(urlPathTemplate("/query/style_form/explode_true/object"))
            .apply { queryObject?.let { withQueryParam("queryObject", it) } }
            .configurer()
        )
}
