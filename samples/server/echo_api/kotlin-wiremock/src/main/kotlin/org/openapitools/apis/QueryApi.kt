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

open class QueryApiStubs(protected val objectMapper: ObjectMapper) {

    fun testEnumRefString(enumNonrefStringQuery: StringValuePattern? = null, enumRefStringQuery: StringValuePattern? = null, configurer: MappingBuilder.() -> MappingBuilder = { this }): TestEnumRefStringStubBuilder =
        TestEnumRefStringStubBuilder(get("/query/enum_ref_string")
            .apply { enumNonrefStringQuery?.let { withQueryParam("enumNonrefStringQuery", it) } }
            .apply { enumRefStringQuery?.let { withQueryParam("enumRefStringQuery", it) } }
            .configurer()
        )

    inner class TestEnumRefStringStubBuilder(private val stub: MappingBuilder) {

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

    fun testQueryDatetimeDateString(datetimeQuery: StringValuePattern? = null, dateQuery: StringValuePattern? = null, stringQuery: StringValuePattern? = null, configurer: MappingBuilder.() -> MappingBuilder = { this }): TestQueryDatetimeDateStringStubBuilder =
        TestQueryDatetimeDateStringStubBuilder(get("/query/datetime/date/string")
            .apply { datetimeQuery?.let { withQueryParam("datetimeQuery", it) } }
            .apply { dateQuery?.let { withQueryParam("dateQuery", it) } }
            .apply { stringQuery?.let { withQueryParam("stringQuery", it) } }
            .configurer()
        )

    inner class TestQueryDatetimeDateStringStubBuilder(private val stub: MappingBuilder) {

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

    fun testQueryIntegerBooleanString(integerQuery: StringValuePattern? = null, booleanQuery: StringValuePattern? = null, stringQuery: StringValuePattern? = null, configurer: MappingBuilder.() -> MappingBuilder = { this }): TestQueryIntegerBooleanStringStubBuilder =
        TestQueryIntegerBooleanStringStubBuilder(get("/query/integer/boolean/string")
            .apply { integerQuery?.let { withQueryParam("integerQuery", it) } }
            .apply { booleanQuery?.let { withQueryParam("booleanQuery", it) } }
            .apply { stringQuery?.let { withQueryParam("stringQuery", it) } }
            .configurer()
        )

    inner class TestQueryIntegerBooleanStringStubBuilder(private val stub: MappingBuilder) {

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

    fun testQueryStyleDeepObjectExplodeTrueObject(queryObject: StringValuePattern? = null, configurer: MappingBuilder.() -> MappingBuilder = { this }): TestQueryStyleDeepObjectExplodeTrueObjectStubBuilder =
        TestQueryStyleDeepObjectExplodeTrueObjectStubBuilder(get("/query/style_deepObject/explode_true/object")
            .apply { queryObject?.let { withQueryParam("queryObject", it) } }
            .configurer()
        )

    inner class TestQueryStyleDeepObjectExplodeTrueObjectStubBuilder(private val stub: MappingBuilder) {

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

    fun testQueryStyleFormExplodeTrueArrayString(queryObject: StringValuePattern? = null, configurer: MappingBuilder.() -> MappingBuilder = { this }): TestQueryStyleFormExplodeTrueArrayStringStubBuilder =
        TestQueryStyleFormExplodeTrueArrayStringStubBuilder(get("/query/style_form/explode_true/array_string")
            .apply { queryObject?.let { withQueryParam("queryObject", it) } }
            .configurer()
        )

    inner class TestQueryStyleFormExplodeTrueArrayStringStubBuilder(private val stub: MappingBuilder) {

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

    fun testQueryStyleFormExplodeTrueObject(queryObject: StringValuePattern? = null, configurer: MappingBuilder.() -> MappingBuilder = { this }): TestQueryStyleFormExplodeTrueObjectStubBuilder =
        TestQueryStyleFormExplodeTrueObjectStubBuilder(get("/query/style_form/explode_true/object")
            .apply { queryObject?.let { withQueryParam("queryObject", it) } }
            .configurer()
        )

    inner class TestQueryStyleFormExplodeTrueObjectStubBuilder(private val stub: MappingBuilder) {

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
