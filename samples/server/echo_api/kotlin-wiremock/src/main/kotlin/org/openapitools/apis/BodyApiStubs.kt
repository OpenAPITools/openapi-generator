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

open class BodyApiStubs(private val objectMapper: ObjectMapper) {

    fun testBinaryGif(configurer: MappingBuilder.() -> MappingBuilder = { this }): TestBinaryGifStubBuilder =
        TestBinaryGifStubBuilder(objectMapper, post(urlPathTemplate("/binary/gif"))
            .configurer()
        )

    fun testBodyApplicationOctetstreamBinary(configurer: MappingBuilder.() -> MappingBuilder = { this }): TestBodyApplicationOctetstreamBinaryStubBuilder =
        TestBodyApplicationOctetstreamBinaryStubBuilder(objectMapper, post(urlPathTemplate("/body/application/octetstream/binary"))
            .configurer()
        )

    fun testBodyMultipartFormdataArrayOfBinary(configurer: MappingBuilder.() -> MappingBuilder = { this }): TestBodyMultipartFormdataArrayOfBinaryStubBuilder =
        TestBodyMultipartFormdataArrayOfBinaryStubBuilder(objectMapper, post(urlPathTemplate("/body/application/octetstream/array_of_binary"))
            .configurer()
        )

    fun testBodyMultipartFormdataSingleBinary(configurer: MappingBuilder.() -> MappingBuilder = { this }): TestBodyMultipartFormdataSingleBinaryStubBuilder =
        TestBodyMultipartFormdataSingleBinaryStubBuilder(objectMapper, post(urlPathTemplate("/body/application/octetstream/single_binary"))
            .configurer()
        )

    fun testEchoBodyFreeFormObjectResponseString(configurer: MappingBuilder.() -> MappingBuilder = { this }): TestEchoBodyFreeFormObjectResponseStringStubBuilder =
        TestEchoBodyFreeFormObjectResponseStringStubBuilder(objectMapper, post(urlPathTemplate("/echo/body/FreeFormObject/response_string"))
            .configurer()
        )

    fun testEchoBodyPet(configurer: MappingBuilder.() -> MappingBuilder = { this }): TestEchoBodyPetStubBuilder =
        TestEchoBodyPetStubBuilder(objectMapper, post(urlPathTemplate("/echo/body/Pet"))
            .configurer()
        )

    fun testEchoBodyPetResponseString(configurer: MappingBuilder.() -> MappingBuilder = { this }): TestEchoBodyPetResponseStringStubBuilder =
        TestEchoBodyPetResponseStringStubBuilder(objectMapper, post(urlPathTemplate("/echo/body/Pet/response_string"))
            .configurer()
        )

    fun testEchoBodyTagResponseString(configurer: MappingBuilder.() -> MappingBuilder = { this }): TestEchoBodyTagResponseStringStubBuilder =
        TestEchoBodyTagResponseStringStubBuilder(objectMapper, post(urlPathTemplate("/echo/body/Tag/response_string"))
            .configurer()
        )
}
